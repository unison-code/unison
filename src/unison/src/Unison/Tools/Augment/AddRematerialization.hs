{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Augment.AddRematerialization (addRematerialization) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Graphs.Hoopl
import Unison.Graphs.Hoopl.ReachingConstants

addRematerialization f target =
    let bif    = branchInfo target
        f'    = basicForm f
        cfg   = toHGraph bif f'
        ts    = map undoPreAssign $ tUniqueOps (flatCode f')
        cs f  = M.fromList $
                concatMap (inCongruents (fCongruences f)) (fCode f)
        t2cs  = cs f'
        rts   = M.toList $ reachingConstants cfg ts t2cs
        rts'  = filter (isUsefulRemat f) rts
        rts'' = [(t, o) | (t, [o]) <- rts']
        p2ts  = M.fromList
                [(operandId p, altTemps p) | p <- codeOperands f]
        p2cts = M.map (concatMap (\(OperandRef pid) -> p2ts M.! pid)) $
                M.mapKeys toCongruenceOp $
                cs f
        f''   = foldl (addRemat p2cts) f rts''
    in f''

basicForm f @ Function {fCode = code, fCongruences = cs} =
    let code1 = map filterMandatory code
        p2t   = M.fromList $ concatMap operandToTemp code1
        code2 = map (mapToOperationInBlock (mapToModelOperand (applyOpMap p2t)))
                code1
        cs1   = sort (map (mapTuple (applyOpMap p2t)) cs)
    in f {fCode = code2, fCongruences = cs1}

filterMandatory b @ Block {bCode = code} =
    let code1 = filter isMandatory code
        ts    = map undoPreAssign $ concatMap defTemps code1
        code2 = map (mapToModelOperand (filterTemps ts)) code1
    in b {bCode = code2}

filterTemps ts p @ MOperand {altTemps = ts'} = p {altTemps = intersect ts' ts}

useTemps = concat . map extractTemps . oUses
defTemps = concat . map extractTemps . oDefs

operandToTemp Block {bCode = code} = concatMap (map opToTemp . oModelOps) code

opToTemp MOperand {operandId = p, altTemps = [t]} = (p, t)
opToTemp foo = error ("unmatched: opToTemp " ++ show foo)

applyOpMap p2t MOperand {operandId = p} = p2t M.! p
applyOpMap p2t (OperandRef p) = p2t M.! p

inCongruents cs b = [(t, tempCongruents t cs) | t <- oDefs (blockIn b)]
tempCongruents t cs = [t' | (t', t'') <- cs, toCongruenceOp t == t'']

isUsefulRemat Function {fCode = code} rt = any (isUsefulRematInBlock rt) code

isUsefulRematInBlock rt Block {bCode = code} =
  any (isUsefulRematInOpr code rt) code

isUsefulRematInOpr code (t, _) o
  | isCopy o = False
  -- TODO: why is this the case? shouldn't we rather check whether o uses a
  -- temporary copied from t through a virtual copy?
  | isOut o = isPotentialUser t o &&
              any (\u -> isDerivedFromVirtualCopy u && isPotentialUser t u) code
  | isPotentialUser t o && not (isFirstRealUser code t o) = True
  | otherwise = False

addRemat p2cts f @ Function {fCode = code} rt @ (t, _) =
    let fcode = flatten code
        ids   = (newOprIndex fcode, newOperIndex fcode, newTempIndex fcode)
        -- add rematerialization operations for each use
        (_, code'')  = mapAccumL (addRematToBlock rt) ids code
        -- make the definer operands nullable across the function
        (_, code''') = fixpoint (nullifyOperands p2cts) ([t], code'')
        f'    = f {fCode = code'''}
    in f'

addRematToBlock rt ids b @ Block {bCode = code} =
    let ((ids', _), code') =
          mapAccumL (maybeAddRematToOpr code rt) (ids, []) code
    in (ids', b {bCode = concat code'})

maybeAddRematToOpr code rt acc o
  | isUsefulRematInOpr code rt o = addRematToOpr rt acc o
  | otherwise = (acc, [o])

isFirstRealUser code t o
  | isIn (potentialDefiner t code) = False
  | otherwise =
      case find (isRealUser t) code of
        Nothing -> True
        Just o' -> o == o'

isRealUser t o = any (isRealConnection t) (oUseOperands o)

addRematToOpr (t, pre) ((oid, pid, tid), ts) o =
    let t'    = mkTemp tid
        ts'   = ts ++ [t']
        pre1  = pre {oId = oid}
        p'    = mkMOperand pid [t'] Nothing
        pre2  = mapToOperandIf ((==) (mkTemp 0)) (const p') pre1
        pre3  = makeOptional pre2
        pre4  = mapToAttrRemat (const True) pre3
        o'    = mapToOperandIf
                (\p -> isMOperand p && t `elem` altTemps p)
                (\p -> p {altTemps = altTemps p ++ ts'}) o
    in (((oid + 1, pid + 1, tid + 1), ts'), [pre4, o'])

nullifyOperands _ ([], code) = ([], code)
-- 1. nullify operands defining t, add temps congruent to t to cts
-- 2. nullify (out) operands using any of cts
nullifyOperands p2cts (t:ts, code) =
    let (cts, code') = unzip $ map (nullifyDefOperandsInBlock p2cts t) code
        cts' = concat cts
        code'' = foldl nullifyOutOperands code' cts'
    in (ts ++ cts', code'')

nullifyDefOperandsInBlock p2cts t b @ Block {bCode = code} =
  let (ts, code') = unzip $ map (nullifyDefOperandsInOpr code p2cts t) code
  in (concat ts, b {bCode = code'})

nullifyDefOperandsInOpr code p2cts t o
  | not (isSureDefiner t o) = ([], o)
  | isIn o =
    let o' = mapToModelOperand (nullifyOperand t) o
        p  = fromJust $ find (\p -> t `elem` altTemps p) (oAllOperands o)
        ts = p2cts M.! (toCongruenceOp p)
    in (ts, o')
  -- a sure definer of t that is not (in) must necessarily be the original one,
  -- nullify only if it is not directly followed by a real use
  | none (isRealUser t) code =
    let o' = makeOptional o
        ts = useTemps o
    in if null ts then (ts, o') else
          error ("TODO: handle nullification of multi-operation rematerialization")
  | otherwise = ([], o)

nullifyOutOperands code t = map (nullifyOutOperandsInBlock t) code

nullifyOutOperandsInBlock t b @ Block {bCode = code} =
  let u = blockOut b
  in if isPotentialUser t u then
       let code' = nullifyOperandOf (oId u) t code
       in b {bCode = code'}
  else b

nullifyOperandOf oid t code =
    mapIf (\o -> oId o == oid) (mapToModelOperand (nullifyOperand t)) code

nullifyOperand t p
  | isRealConnection t p = addNullTemp p
  | otherwise = p

isRealConnection t p @ MOperand {altTemps = ts} =
    t `elem` ts && not (isNullableOperand p)

codeOperands f = concatMap oAllOperands (flatCode f)

isSureDefiner t o = any (isSureConnection t) (oDefs o)

isSureConnection t MOperand {altTemps = [t']} = t == t'
isSureConnection _ _ = False
