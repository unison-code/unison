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
module Unison.Tools.Import.LiftUndefRegs (liftUndefRegs) where

import Data.List
import Unison

liftUndefRegs f @ Function {fCode = code} _target =
    let code' = mapToEntryBlock (appendToIn (orphans $ flatten code)) code
    in f {fCode = code'}

-- | Registers that are used but not defined
orphans code =
    filter isRegister (nub (concatMap oUses code) \\ nub (concatMap oDefs code))
