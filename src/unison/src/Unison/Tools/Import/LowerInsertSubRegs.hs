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
module Unison.Tools.Import.LowerInsertSubRegs (lowerInsertSubRegs) where

import MachineIR
import Unison
import Unison.Target.API

lowerInsertSubRegs mf target =
  let stf      = subRegIndexType target
      newId    = newMachineTempId mf
      (mf', _) = traverseMachineFunction (lowerInsertInstrSubRegs stf) newId mf
  in mf'

lowerInsertInstrSubRegs stf (accIs, id) (mi @
  MachineSingle {msOperands = [d @ MachineTemp {},
                               s1 @ MachineTemp {},
                               s2 @ MachineTemp {},
                               sr]}
  : is)
  | isMachineInsertSubReg mi =
  let subreg     = toSubRegIndex sr
      (id', mis) =
        case stf subreg of
          LowSubRegIndex ->
            let t  = mkMachineTemp id Nothing
                hi = mi {msOpcode = mkMachineVirtualOpc HIGH,
                         msOperands = [t, s1]}
                co = mi {msOpcode = mkMachineVirtualOpc COMBINE,
                         msOperands = [d, s2, t]}
            in (id + 1, [hi, co])
          HighSubRegIndex ->
            let t  = mkMachineTemp id Nothing
                lo = mi {msOpcode = mkMachineVirtualOpc LOW,
                                    msOperands = [t, s1]}
                co = mi {msOpcode = mkMachineVirtualOpc COMBINE,
                                    msOperands = [d, t, s2]}
            in (id + 1, [lo, co])
          CopySubRegIndex -> (id, [mi {msOpcode = mkMachineVirtualOpc COPY,
                                       msOperands = [d, s2]}])
  in lowerInsertInstrSubRegs stf (accIs ++ mis, id') is

lowerInsertInstrSubRegs stf (accIs, id) (mi : is) =
  lowerInsertInstrSubRegs stf (accIs ++ [mi], id) is

lowerInsertInstrSubRegs _ (is, acc) [] = (is, acc)
