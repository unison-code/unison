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