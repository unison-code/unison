{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

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