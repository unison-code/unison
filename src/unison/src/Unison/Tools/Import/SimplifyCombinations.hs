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

import Common.Util
import Unison

instance Eq i => Eq (Block i r)
  where b == b' = and [eqType i i' | (i, i') <- zip (bCode b) (bCode b')]

eqType i i' = oType i == oType i'

simplifyCombinations f  @ Function {fCode = code} _target =
    let code'    = map (fixpoint (simplifyExtract LowType)) code
        code''   = map (fixpoint (simplifyExtract HighType)) code'
        code'''  = fixpoint simplifyDefine code''
    in f {fCode = code'''}

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
  [t16] <- (define) []
-}

simplifyDefine code =
  let fcode  = flatten code
      isExtr = \o -> isHigh o || isLow o
  in case find (\o -> isExtr o && isOnlyDefUser fcode o) fcode of
    (Just e) ->
      let d      = singleUseDefiner e fcode
          code'  = filterCode (not . isIdOf d) code
          code'' = mapToOperationInBlocks (mkDefineIf (isIdOf e)) code'
      in code''
    Nothing  -> code

isOnlyDefUser fcode o =
  let d = singleUseDefiner o fcode
  in isDefine d && length (users (oSingleUse o) fcode) == 1

singleUseDefiner o code = definer (oSingleUse o) code

mkDefineIf p o
  | p o = mkDefine (oId o) (oDefs o)
  | otherwise = o
