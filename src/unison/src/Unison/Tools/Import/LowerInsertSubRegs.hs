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
  MachineSingle {msOperands = [MachineTemp {mtId = did}, _, _, sr]} : is)
  | isMachineInsertSubReg mi =
  let subreg                    = toSubRegIndex sr
      subops                    = stf (tid2rc M.! did) subreg
      (id', mis)                = lowerInsertInstrSubRegs' subops id mi
  in lowerInsertInstrSubRegs stf tid2rc (accIs ++ mis, id') is

lowerInsertInstrSubRegs stf tid2rc (accIs, id) (mi @
  MachineSingle {msOperands = [d @ MachineTemp {mtId = did},
                               _,
                               s2 @ MachineTemp {}, sr]} : is)
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

lowerInsertInstrSubRegs' [CopySubRegIndex] id mi @ MachineSingle {msOperands = [d, _, s2, _]}
  = let cp = mi {msOpcode = mkMachineVirtualOpc COPY, msOperands = [d, s2]}
  in (id, [cp])
lowerInsertInstrSubRegs' [LowSubRegIndex] id mi @ MachineSingle {msOperands = [d, s1, s2, _]}
  = let t  = mkSimpleMachineTemp id
        hi = mi {msOpcode = mkMachineVirtualOpc HIGH, msOperands = [t, s1]}
        co = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [d, s2, t]}
    in (id + 1, [hi, co])
lowerInsertInstrSubRegs' [HighSubRegIndex] id mi @ MachineSingle {msOperands = [d, s1, s2, _]}
  = let t  = mkSimpleMachineTemp id
        lo = mi {msOpcode = mkMachineVirtualOpc LOW, msOperands = [t, s1]}
        co = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [d, s2, t]}
    in (id + 1, [lo, co])
lowerInsertInstrSubRegs' [LowSubRegIndex, LowSubRegIndex] id mi @ MachineSingle {msOperands = [d, s1, s2, _]}
  = let t     = mkSimpleMachineTemp id
        t'    = mkSimpleMachineTemp (id + 1)
        t''   = mkSimpleMachineTemp (id + 2)
        t'''  = mkSimpleMachineTemp (id + 3)
        lo    = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t, s1]}
        hi    = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t', t]}
        co    = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [t'', s2, t']}
        hi'   = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t''', s1]}
        co'   = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [d, t'', t''']}
    in (id + 4, [lo, hi, co, hi', co'])
lowerInsertInstrSubRegs' [HighSubRegIndex, LowSubRegIndex] id mi @ MachineSingle {msOperands = [d, s1, s2, _]}
  = let t     = mkSimpleMachineTemp id
        t'    = mkSimpleMachineTemp (id + 1)
        t''   = mkSimpleMachineTemp (id + 2)
        t'''  = mkSimpleMachineTemp (id + 3)
        lo    = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t, s1]}
        lo'   = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t', t]}
        co    = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [t'', t', s2]}
        hi    = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t''', s1]}
        co'   = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [d, t'', t''']}
    in (id + 4, [lo, lo', co, hi, co'])
lowerInsertInstrSubRegs' [LowSubRegIndex, LowSubRegIndex, LowSubRegIndex] id mi @ MachineSingle {msOperands = [d, s1, s2, _]}
  = let t       = mkSimpleMachineTemp id
        t'      = mkSimpleMachineTemp (id + 1)
        t''     = mkSimpleMachineTemp (id + 2)
        t'''    = mkSimpleMachineTemp (id + 3)
        t''''   = mkSimpleMachineTemp (id + 4)
        t'''''  = mkSimpleMachineTemp (id + 5)
        t'''''' = mkSimpleMachineTemp (id + 6)
        lo      = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t, s1]}
        lo'     = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t', t]}
        hi      = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t'', t']}
        co      = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [t''', s2, t'']}
        hi'     = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t'''', t]}
        co'     = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [t''''', t''', t'''']}
        hi''    = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t'''''', s1]}
        co''    = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [d, t''''', t'''''']}
    in (id + 7, [lo, lo', hi, co, hi', co', hi'', co''])
lowerInsertInstrSubRegs' [HighSubRegIndex, LowSubRegIndex, LowSubRegIndex] id mi @ MachineSingle {msOperands = [d, s1, s2, _]}
  = let t       = mkSimpleMachineTemp id
        t'      = mkSimpleMachineTemp (id + 1)
        t''     = mkSimpleMachineTemp (id + 2)
        t'''    = mkSimpleMachineTemp (id + 3)
        t''''   = mkSimpleMachineTemp (id + 4)
        t'''''  = mkSimpleMachineTemp (id + 5)
        t'''''' = mkSimpleMachineTemp (id + 6)
        lo      = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t, s1]}
        lo'     = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t', t]}
        lo''    = mi {msOpcode = mkMachineVirtualOpc LOW,     msOperands = [t'', t']}
        co      = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [t''', t'', s2]}
        hi      = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t'''', t]}
        co'     = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [t''''', t''', t'''']}
        hi'     = mi {msOpcode = mkMachineVirtualOpc HIGH,    msOperands = [t'''''', s1]}
        co''    = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = [d, t''''', t'''''']}
    in (id + 7, [lo, lo', lo'', co, hi, co', hi', co''])
