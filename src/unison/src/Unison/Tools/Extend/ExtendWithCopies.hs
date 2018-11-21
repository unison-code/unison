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
module Unison.Tools.Extend.ExtendWithCopies (extendWithCopies) where

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow

import Common.Util

import Unison
import Unison.Target.API
import Unison.Target.RegisterArray
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.Partition as P
import Unison.Analysis.CallingConvention

extendWithCopies f target =
  let cf        = copies target
      apf       = alignedPairs target
      bif       = branchInfo target
      csf       = calleeSaved target
      rif       = rematInstrs target
      ra        = mkRegisterArray target 0
      ovf       = regOverlap (regAtoms ra)
      cst       = calleeSavedTemps csf ovf f
      cg        = CG.fromFunction f
      bcfg      = BCFG.fromFunction bif f
      sg        = SG.fromFunction (Just apf) f
      fInfo     = (f, cst, cg, ra, bcfg, sg)
      rtmap     = mkRematTempMap rif f
      id        = newId (fCode f)
      t2rs      = mkMustRegistersMap f
      (f1, id') = foldWithTempIndex
                  (extendBB rtmap True virtualTemps always (cf fInfo) t2rs)
                  (f {fCode = []}, id) (fCode f)
      code''    = filterCode (not . isVirtualCopy) (fCode f1)
      f2        = f1 {fCode = code''}
      cg'       = CG.fromFunction f2
      bcfg'     = BCFG.fromFunction bif f2
      sg'       = SG.fromFunction (Just apf) f2
      fInfo'    = (f2, cst, cg', ra, bcfg', sg')
      t2rs'     = mkMustRegistersMap f2
      (f3, _)   = foldWithTempIndex
                  (extendBB rtmap False allTemps notRCopy (cf fInfo') t2rs')
                  (f2 {fCode = []}, id') (fCode f2)
      f4        = updateRematOrigins rif f3
  in f4

-- | Extends the block temporaries given by the function ft with copies
extendBB rtmap vc ft rf cf t2rs
  (ti, (f @ Function {fCode = accCode, fCongruences = cs}, id))
  b @ Block {bCode = code} =
  let (ids, itf)                = ft code
      init                      = (ti, code, [], id, t2rs)
      (ti', code', irs, id', _) = foldl
                                  (extendTemporary vc rtmap itf rf cf) init ids
      cs'                       = updateSame cs irs
  in (ti', (f {fCode = accCode ++ [b {bCode = code'}], fCongruences = cs'}, id'))

-- | Gives a list of ids and a function from ids to all temporary pairs of the
-- | form (t, t)
allTemps code =
    let id2t = zip [0..] [(t, t) | t <- tUniqueOps code]
        itf  = applyTempMap (M.fromList id2t)
        ids  = fst $ unzip id2t
    in (ids, itf)

applyTempMap m id _ = m M.! id

notRCopy = not . isNonVirtualCopy

-- | Gives a list of ids and a function from ids to pairs of temporaries in
-- | virtual copies
virtualTemps code = ([oId i | i <- code, isVirtualCopy i], virtualCopyTemps)

virtualCopyTemps id = copyOps . findBy id oId

-- | Extends the given temporary pair. The tuple (src, dst) contains different
-- | temporaries only when virtual copy temporaries are extended
extendTemporary vc rtmap itf rf cf (ti, code, irs, id, t2rs) oId =
    let (src, dst) = itf oId code
        code'      = filter rf code
        d          = find (isDefiner src) code'
        us         = filter (isUser dst) code'
    in extendReferences vc rtmap cf (src, dst) d us (ti, code, irs, id, t2rs)

extendReferences _ _ _ _ Nothing _  out = out
extendReferences _ _ _ _ _       [] out = out
extendReferences vc rtmap cf (src, dst) (Just d) us (ti, code, irs, id, t2rs) =
    let rs         = mustRegisters t2rs src
        (dcs, ucs) = cf vc src rs d us
        (dcs',
         ucs') = case M.lookup src rtmap of
                  Just (_, ris) -> rematCopies src code ris (dcs, ucs) vc (d, us)
                  Nothing -> (dcs, ucs)
        ro         = case M.lookup src rtmap of
                      Just (ro':_, _) -> Just ro'
                      Nothing -> Nothing
        t'         = if null dcs' then src else mkTemp ti
        extDefOut  = extend vc ro undefT src after (ti, code, [], id, t2rs)
                     (d, dcs')
        (ti', code',
         irs', id',
         t2rs')    = foldl (extend vc ro dst t' before) extDefOut
                     (zip us ucs')
    in (ti', code', irs ++ irs', id', t2rs')

-- if the temporary can be defined by a 'X_source' instruction and is only
-- used once by an (out) operation, 'X_source' is enough to rematerialize,
-- no need for other remat copies
rematCopies t code _ (dcs, ucs) vc (d, [u])
  -- additionally, the copy cannot be virtual and there must be really one
  -- user (virtual or not): 'users t code' includes also virtual copies
  | not (isIn d) && isOut u && not vc && length (users t code) == 1 = (dcs, ucs)
-- general case
rematCopies _ _ ris (dcs, ucs) _ _ =
  (if null dcs then []
   else dcs ++ [TargetInstruction drc | (_, drc, _) <- ris],
   [if null ucs0 then []
    else ucs0 ++ [TargetInstruction urc | (_, _, urc) <- ris]
   | ucs0 <- ucs])

-- | Updates the same tuples according to the irs tuples
updateSame same irs =
  let s     = M.fromList [(undoPreAssign t, undoPreAssign t')
                         | (i, (t, t')) <- irs, isOut i]
      same' = map (first (applyMap s)) same
  in same'

updateTemps (firstT, newT) oprToExtend code =
    let tMap   = M.fromList [(tId firstT, tId newT)]
        isInst = isId (oId oprToExtend)
        code'  = mapIf isInst (mapToModelOperand (applyTempIdMap tMap)) code
    in code'

-- | Extends the code according to firstT, prevT and pos
extend _ _ firstT prevT _ (ti, code, irs, id, t2rs) (oprToExtend, []) =
    let r      = (firstT, prevT)
        code'  = updateTemps r oprToExtend code
        t2rs'  = replaceTemp t2rs r
    in (ti, code', irs ++ [(oprToExtend, r)], id, t2rs')

extend vc ro firstT prevT pos (ti, code, irs, id, t2rs) (oprToExtend, insts) =
    let newT   = mkTemp ti
        ti'    = ti + 1
        id'    = id + 1
        copy   = mkCopy id insts (undoPreAssign prevT) [] (undoPreAssign newT) []
        copy1  = mapToAttrVirtualCopy (const vc) copy
        copy2  = mapToAttrRematOrigin (const (fmap oId ro)) copy1
        copy3  = case ro of
                  (Just ro') -> mapToAttrMem (const (aMem $ oAs ro')) copy2
                  Nothing -> copy2
        r      = (firstT, newT)
        code'  = updateTemps r oprToExtend code
        code'' = insertWhen pos (isIdOf oprToExtend) [copy3] code'
    in (ti', code'', irs ++ [(oprToExtend, r)], id', t2rs)

undefT = mkTemp (-1)

-- | Gives a map from a temporary to a list of registers such that either that
-- temporary or a congruent one must be placed in the register(s)
mkMustRegistersMap f @ Function {fCode = code} =
    let must   = preAssignments code
        mMap   = combineMustsByTemp must
        sg     = SG.fromFunction Nothing f
        tsToRs = [(ts, nubMaybes [M.lookup (mkTemp t) mMap | t <- ts])
                  | ts <- P.toList sg]
    in M.fromList (concat [[(mkTemp t, rs) | t <- ts] | (ts, rs) <- tsToRs])

nubMaybes :: Eq r => [Maybe [Operand r]] -> [Operand r]
nubMaybes = nub . concat . catMaybes

mustRegisters t2rs t = map (rTargetReg . regId) $ M.findWithDefault [] t t2rs

combineMustsByTemp :: Ord r => [(t1, Operand r, Operand r)] ->
                      M.Map (Operand r) [Operand r]
combineMustsByTemp = M.fromListWith combineMust . mustToAList

mustToAList must = [(t, [r]) | (_, t, r) <- must]

combineMust rs1 = nub . (rs1 ++)

replaceTemp t2rs (oldT, newT)
  | oldT == undefT = t2rs
  | otherwise =
    let oldRs  = t2rs M.! oldT
        t2rs'  = M.delete oldT t2rs
        t2rs'' = M.update (Just . nub . (++) oldRs) newT t2rs'
    in t2rs''

mkRematTempMap rif f @ Function {fRematerializable = rts} =
  let fcode = flatCode f
  in M.fromList (mapMaybe (toRematTemp rif fcode) rts)

toRematTemp rif fcode (t, oids) =
  let os   = map (\oid -> fromJust $ find (isId oid) fcode) oids
      -- All definers should be implemented equally, otherwise t would not
      -- be rematerializable
      ris  = catMaybes [rif i | TargetInstruction i <- oInstructions $ head os]
  in if null ris then Nothing else Just (t, (os, ris))

updateRematOrigins rif f @ Function {fCode = code} =
  let os = S.fromList $ mapMaybe (aRematOrigin . oAs) $ flatten code
      f' = mapToOperation (addDematerialize rif os) f
  in f' {fRematerializable = []}

addDematerialize rif os o
  | S.member (oId o) os =
      let ris = catMaybes [rif i | TargetInstruction i <- oInstructions o]
          is' = [TargetInstruction s | (s, _, _) <- ris]
      in mapToInstructions (\is -> is ++ is') o
  | otherwise = o
