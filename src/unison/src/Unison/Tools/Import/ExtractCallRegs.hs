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
module Unison.Tools.Import.ExtractCallRegs (extractCallRegs) where

import Data.List
import Data.List.Split

import Common.Util

import Unison
import Unison.Target.API
import Unison.Target.RegisterArray

extractCallRegs f @ Function {fCode = code} target =
    let ra  = mkRegisterArray target 0
        rr  = map (mkRegister . mkTargetRegister) $ reserved target
        rs  = nub $ concatMap (raRegisters ra) (raRcs ra)
        rs' = rs \\ rr
        t   = mkTemp $ newTempIndex $ flatten code
        id  = newId code
    in foldFunction (extractCallRegsInCall rs') (t, id) f

extractCallRegsInCall rs (code, (t, id)) o
  | isCall o || isTailCall o =
    let callId    = oId o
        isTheCall = isId callId
        [b,_,a]   = split (whenElt isTheCall) code
        prol      = filter (isPrologueOpr rs) $ dropUntil isPerilogueLimit b
        epil      = filter (isEpilogueOpr rs) $
                    takeWhile (not . (\o -> isPerilogueLimit o ||
                                            isPrologueOpr rs o)) a
        ((t', code'),   pt) = mapAccumL virtualizeProlInstr (t, code) prol
        ((t'', code''), et) = mapAccumL virtualizeEpilInstr (t', code') epil
        fun       = mapToAttrCall (const (Just callId)) (mkFun id pt et)
        code'''   = insertWhen after isTheCall [fun] code''
    in (code''', (t'', id + 1))
  | otherwise = (code, (t, id))

dropUntil f l = reverse $ takeWhile (not . f) (reverse l)

isPerilogueLimit o = isCall o || isFun o || isDelimiter o

isPrologueOpr rs o
    | isCopy o &&
      isTemporary (copySource o) && isRegister (copyDestination o) &&
      copyDestination o `elem` rs = True
    | length (oDefs o) == 1 && isRegister (oSingleDef o) &&
      oSingleDef o `elem` rs = True
    | otherwise = False

isEpilogueOpr rs o
    | isCopy o &&
      isRegister (copySource o) && isTemporary (copyDestination o) &&
      copySource o `elem` rs = True
    | otherwise = False

virtualizeProlInstr (t, code) o =
    let vi    = mkVirtual (oSingleUse o, t) o
        code' = replaceOpr o vi code
        r     = oSingleDef o
    in ((t + 1, code'), preAssign t r)

virtualizeEpilInstr (t, code) o =
    let vi    = mkVirtual (t, oSingleDef o) o
        code' = replaceOpr o vi code
        r     = oSingleUse o
    in ((t + 1, code'), preAssign t r)

mkVirtual (s, d) o
  | isCopy o  = mkVirtualCopy (oId o) s d
  | otherwise = mapToOperands id (const [d]) o

replaceOpr o = mapIf (isId $ oId o) . const
