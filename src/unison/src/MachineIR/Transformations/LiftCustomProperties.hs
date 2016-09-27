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
module MachineIR.Transformations.LiftCustomProperties (liftCustomProperties) where

import Data.List
import MachineIR

liftCustomProperties mf _target = mapToMachineInstruction liftCustomProperty mf

liftCustomProperty mi @ MachineSingle {msOperands = mops, msProperties = mps} =
  case find isMachineProperty mops of
    (Just MachineProperty {mpProperty = p}) ->
      let mops' = filter (not . isMachineProperty) mops
          mps'  = mps ++ [mkMachineInstructionPropertyCustom p]
      in mi {msOperands = mops', msProperties = mps'}
    Nothing -> mi