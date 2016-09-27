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
module Unison.Tools.Import.LiftMemoryPartitions (liftMemoryPartitions) where

import Data.List
import MachineIR

liftMemoryPartitions mf _target = mapToMachineInstruction liftMemoryPartition mf

liftMemoryPartition mi @ MachineSingle {msOperands = mops, msProperties = mps} =
  case find isMachineMemPartition mops of
    (Just MachineMemPartition {mmpId = id}) ->
      let mops' = filter (not . isMachineMemPartition) mops
          mps'  = mps ++ [mkMachineInstructionPropertyMem id]
      in mi {msOperands = mops', msProperties = mps'}
    Nothing -> mi