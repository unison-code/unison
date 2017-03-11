{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Import.SimplifyCombinations (simplifyCombinations) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Common.Util
import Unison

simplifyCombinations f @ Function {fCode = code1}  _target =
    let code2 = fixpoint (foldSimplifyCombines f) code1
        code3 = map (fixpoint (simplifyExtract LowType)) code2
        code4 = map (fixpoint (simplifyExtract HighType)) code3
        code5 = fixpoint simplifyDefine code4
    in f {fCode = code5}

foldSimplifyCombines f code =
  let Function {fCode = code1} = fixpoint foldSimpleCopy (f {fCode = code})
      code2 = fixpoint simplifyCombineComponents code1
  in code2

{-
Transforms:
  [t61] <- (combine) [t60,t60]
  [t17] <- (copy) [t61]
  (where t61 is not used elsewhere)
        <- (high) [t17]
        <- (low) [t17]
        ...
into:
  [t61] <- (combine) [t60,t60]
        <- (high) [t61]
        <- (low) [t61]
-}

foldSimpleCopy :: Show i => Show r => Eq i => Eq r =>
                  Function i r -> Function i r
foldSimpleCopy = foldVirtualCopy isSimpleFoldableVirtualCopy

isSimpleFoldableVirtualCopy code o = isVirtualCopy o && isSimpleFoldable code o

isSimpleFoldable code o =
  let fcode  = flatten code
      (s, d) = (copySource o, copyDestination o)
  in all isSimpleTemp [s, d] &&
     isCombine (definer s fcode) &&
     all isExtract (users d fcode)

isSimpleTemp t = isTemporary t && not (isPreAssigned t)

{-
Transforms:
  [t3] <- (combine) [t1, t2]
  ..
  [t4] <- (low) [t3]
       <- t4
  [t5] <- (high) [t3]
       <- t5
  ..

into:
       <- t1
       <- t2
-}

simplifyCombineComponents code =
  case find (isSimplifiableCombine code) (flatten code) of
   Just c ->
     let (d, l, h) = combineOperands c
         us     = users d (flatten code)
         ts     = M.fromList
                  [(oSingleDef o, if isLow o then l else h) | o <- us]
         code'  = mapToOperationInBlocks
                  (mapToOperands (map (applyMap ts)) id) code
         code'' = foldl removeOpr code' ([c] ++ us)
     in code''
   Nothing -> code

removeOpr code o = filterCode (not . ((==) o)) code

isSimplifiableCombine code o
  | isCombine o =
      let fcode  = flatten code
          (d, l, h) = combineOperands o
      in all (\t -> users t fcode == [o]) [l, h] &&
         all isExtract (users d fcode)
  | otherwise = False

combineOperands SingleOperation {oOpr = Virtual o} =
  (oCombineD o, oCombineLowU o, oCombineHighU o)

{-
Transforms:
  ?
into:
  ?
-}

simplifyExtract eType b @ Block {bCode = code}
    | none (isSimplifiable (isOfType eType) code) code = b
    | otherwise =
      let isExtr = isOfType eType
          e      = fromJust $ find (isSimplifiable isExtr code) code
          id     = oId e
          c      = fromJust $ cDefiner isExtr code e
          copy   = mkVirtualCopy id (subReg eType $ iVirtual c)
                   (oSingleDef e)
          code'  = mapIf (isId id) (const copy) code
      in b {bCode = code'}

isSimplifiable isExtract code i = isJust (cDefiner isExtract code i)

cDefiner isExtract code i
    | isExtract i =
        case find (isDefiner (oSingleUse i)) code of
          (Just c) | isCombine c -> Just c
          _                      -> Nothing
    | otherwise = Nothing

iVirtual SingleOperation {oOpr = Virtual i} = i

isOfType HighType = isHigh
isOfType LowType = isLow

subReg HighType = oCombineHighU
subReg LowType  = oCombineLowU

{-
Transforms:
  [t9] <- (define) []
  [t16] <- (low or high) [t9]
into:
  [t9] <- (define) []
  [t16] <- (define) []
-}

simplifyDefine code =
  let fcode  = flatten code
  in case find (\o -> isExtract o && isDefUser fcode o) fcode of
    (Just e) -> mapToOperationInBlocks (applyIf (isIdOf e) toDefine) code
    Nothing  -> code

isDefUser fcode e =
  let t = oSingleUse e
      d = definer t fcode
  in isDefine d && all isExtract (users t fcode)

toDefine o = mkDefine (oId o) (oDefs o)

isExtract o = isHigh o || isLow o
