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
module MachineIR.Transformations.LiftBlockFreqs (liftBlockFreqs) where

import Data.List
import MachineIR

liftBlockFreqs mf @ MachineFunction {mfBlocks = mbs} _target =
  let mbs' = map liftBlockFreq mbs
      mf'  = mf {mfBlocks = mbs'}
  in mf'

liftBlockFreq mb @ MachineBlock {mbProperties   = mps,
                                 mbInstructions = mis} =
  case partition isMachineBlockFreqInstruction mis of
   ([MachineSingle {msOperands = MachineBlockFreq {mbfFreq = f} : _}], mis') ->
     let mps' = mps ++ [mkMachineBlockPropertyFreq f]
     in mb {mbProperties = mps', mbInstructions = mis'}
   _ -> mb

isMachineBlockFreqInstruction
  MachineSingle {msOpcode = MachineVirtualOpc ANNOTATION_LABEL,
                 msOperands = MachineBlockFreq {} :_} = True
isMachineBlockFreqInstruction _ = False
