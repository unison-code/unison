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
module Unison.Tools.Import.LowerCalls (lowerCalls) where

import Unison.Target.API
import MachineIR

lowerCalls mf target =
  let itf = instructionType target
      oif = operandInfo target
      mf' = mapToMachineBlock (lowerCallInBlock (itf, oif)) mf
  in mf'

lowerCallInBlock fs mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = concatMap (lowerCall fs) mis}

lowerCall (itf, oif) mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                         msOperands = mos}
  | isMachineCallOrTailCall itf mi =
    let oi  = oif i
        n   = length (fst oi) + length (snd oi)
        (cmos, fmos) = splitAt n mos
        mi' = mi {msOperands = cmos}
        mf  = mkMachineSingle (mkMachineVirtualOpc FUN) []
              (filter isMachineReg fmos)
    in [mi', mf]

lowerCall _ mi = [mi]
