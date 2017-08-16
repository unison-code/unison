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
module Unison.Tools.Import.LiftUndefRegs (liftUndefRegs) where

import Data.List
import Unison

liftUndefRegs f @ Function {fCode = code} _target =
    let code' = mapToEntryBlock (appendToIn (orphans $ flatten code)) code
    in f {fCode = code'}

-- | Registers that are used but not defined
orphans code =
    filter isRegister (nub (concatMap oUses code) \\ nub (concatMap oDefs code))
