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
module Unison.Tools.Import.DropDebugLocations (dropDebugLocations) where

import MachineIR

-- Note that this pass is temporary, eventually we need to handle debug
-- information

dropDebugLocations mf _target =
    mapToMachineInstruction dropDebugLocationsInInstr mf

dropDebugLocationsInInstr mi @ MachineSingle {msOperands = mos} =
  let mos'  = filter (not . isMachineDebugLocation) mos
  in mi {msOperands = mos'}
