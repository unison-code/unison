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
module Unison.Tools.Import.LowerSubRegVirtuals (lowerSubRegVirtuals) where

import MachineIR
import Unison
import Unison.Target.API

lowerSubRegVirtuals mf @ MachineFunction {mfBlocks = mbs} target =
  let stf  = subRegIndexType target
      mbs' = map (lowerSubRegVirtualsInBlock stf) mbs
      mf'  = mf {mfBlocks = mbs'}
  in mf'

lowerSubRegVirtualsInBlock stf mb @ MachineBlock {mbInstructions = mis} =
  let mis' = map (lowerSubRegVirtual stf) mis
  in mb {mbInstructions = mis'}

lowerSubRegVirtual stf mi @
  MachineSingle {msOperands = [d @ MachineTemp {},
                               s @ MachineTemp {}, sr]}
  | isMachineExtractSubReg mi =
    let subreg = toSubRegIndex sr
        opcode =
          case stf subreg of
            LowSubRegIndex -> LOW
            HighSubRegIndex -> HIGH
            CopySubRegIndex -> COPY
    in mi {msOpcode = mkMachineVirtualOpc opcode, msOperands = [d, s]}

lowerSubRegVirtual stf mi @
  MachineSingle {msOperands = [d @ MachineTemp {},
                               s1 @ MachineTemp {}, sr1,
                               s2 @ MachineTemp {}, sr2]}
  | isMachineRegSequence mi =
    let subreg1  = toSubRegIndex sr1
        subreg2  = toSubRegIndex sr2
        operands = case (stf subreg1, stf subreg2) of
                     (LowSubRegIndex, HighSubRegIndex) -> ([d, s1, s2])
                     (HighSubRegIndex, LowSubRegIndex) -> ([d, s2, s1])
    in mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = operands}

lowerSubRegVirtual _ mi = mi
