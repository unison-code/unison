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
module Unison.Tools.Import.EnforceCallerSaved (enforceCallerSaved) where

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Target.RegisterArray

enforceCallerSaved f @ Function {fCode = code} target =
    let csr   = map (mkRegister . mkTargetRegister) $ callerSaved target
        ra    = mkRegisterArray target 0
        r2as  = regAtoms ra
        a2mrs = atomToMatchingRegs r2as
        t     = newTempIndex $ flatten code
    in foldFunction (addCallerSaved (r2as, a2mrs) csr) t f

addCallerSaved regInfo csr (code, t) i
  | isFun i && not (isTailCallFun code i) =
    let ds     = oDefs i
        par    = [fromJust (tReg t) | t <- ds, isPreAssigned t]
        ucsr   = additionalRegisters regInfo par csr
        tr     = zip [t..] ucsr
        ds'    = [mkPreAssignedTemp t r | (t, r) <- tr]
        t'     = if null tr then t else fst (last tr) + 1
        code'  = mapIf (i ==) (addFunctionOperands [] ds') code
    in (code', t')
  | otherwise = (code, t)

addFunctionOperands us ds (bi @ SingleOperation {oOpr = Virtual o @ Fun {}}) =
  bi {oOpr = Virtual o {oFunctionUs = (oFunctionUs o) ++ us,
                         oFunctionDs = (oFunctionDs o) ++ ds}}

-- | Gives the registers to be added to rs to cover the register atoms of rs'
additionalRegisters (r2as, a2mrs) rs rs' =
  let toAtomSet = S.fromList . concatMap ((M.!) r2as)
      rsa       = toAtomSet rs
      rsa'      = toAtomSet rs'
  in atomsToRegs a2mrs (S.toList $ S.difference rsa' rsa)
