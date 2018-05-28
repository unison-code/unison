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

import qualified Data.Map as M

lowerInsertSubRegs mf target =
  let stf      = subRegIndexType target
      tid2rc   = registerClassMap mf
      newId    = newMachineTempId mf
      (mf', _) = traverseMachineFunction (lowerInsertInstrSubRegs stf tid2rc)
                 newId mf
  in mf'

lowerInsertInstrSubRegs stf tid2rc (accIs, id) (mi @
  MachineSingle {msOperands = [d @ MachineTemp {mtId = did},
                               s1,
                               s2 @ MachineTemp {},
                               sr]}
  : is)
  | isMachineInsertSubReg mi =
  let subreg     = toSubRegIndex sr
      (id', mis) =
        case stf (tid2rc M.! did) subreg of
          [LowSubRegIndex] ->
            let t  = mkSimpleMachineTemp id
                hi = mi {msOpcode = mkMachineVirtualOpc HIGH,
                         msOperands = [t, s1]}
                co = mi {msOpcode = mkMachineVirtualOpc COMBINE,
                         msOperands = [d, s2, t]}
            in (id + 1, [hi, co])
          [HighSubRegIndex] ->
            let t  = mkSimpleMachineTemp id
                lo = mi {msOpcode = mkMachineVirtualOpc LOW,
                         msOperands = [t, s1]}
                co = mi {msOpcode = mkMachineVirtualOpc COMBINE,
                         msOperands = [d, t, s2]}
            in (id + 1, [lo, co])
          [CopySubRegIndex] -> (id, [mi {msOpcode = mkMachineVirtualOpc COPY,
                                       msOperands = [d, s2]}])
  in lowerInsertInstrSubRegs stf tid2rc (accIs ++ mis, id') is
  | isMachineSubregToReg mi =
  let subreg     = toSubRegIndex sr
      (id', mis) =
        case stf (tid2rc M.! did) subreg of
          [CopySubRegIndex] -> (id, [mi {msOpcode = mkMachineVirtualOpc COPY,
                                       msOperands = [d, s2]}])
          sri ->
            let t   = mkSimpleMachineTemp id
                de  = mi {msOpcode = mkMachineVirtualOpc IMPLICIT_DEF,
                          msOperands = [t]}
                mos = case sri of
                       [LowSubRegIndex]  -> [d, s2, t]
                       [HighSubRegIndex] -> [d, t, s2]
                co = mi {msOpcode = mkMachineVirtualOpc COMBINE,
                         msOperands = mos}
            in (id + 1, [de, co])
  in lowerInsertInstrSubRegs stf tid2rc (accIs ++ mis, id') is

lowerInsertInstrSubRegs stf tid2rc (accIs, id) (mi : is) =
  lowerInsertInstrSubRegs stf tid2rc (accIs ++ [mi], id) is

lowerInsertInstrSubRegs _ _ (is, acc) [] = (is, acc)
