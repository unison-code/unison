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
module Unison.Tools.Import.LowerSubRegVirtuals (lowerSubRegVirtuals) where

import MachineIR
import Unison
import Unison.Target.API

import qualified Data.Map as M

lowerSubRegVirtuals mf @ MachineFunction {} target =
  let stf      = subRegIndexType target
      tid2rc   = registerClassMap mf
      newId    = newMachineTempId mf
      (mf', _) = traverseMachineFunction (lowerSubRegVirtual stf tid2rc)
                 newId mf
  in mf'

lowerSubRegVirtual stf tid2rc (accIs, id) (mi @
  MachineSingle {msOperands = [d @ MachineTemp {},
                               s @ MachineTemp {mtId = sid}, sr]} : is)
  | isMachineExtractSubReg mi =
    let subreg = toSubRegIndex sr
        opcode =
          case stf (tid2rc M.! sid) subreg of
            [LowSubRegIndex]  -> LOW
            [HighSubRegIndex] -> HIGH
            [CopySubRegIndex] -> COPY
        mi' = mi {msOpcode = mkMachineVirtualOpc opcode, msOperands = [d, s]}
    in lowerSubRegVirtual stf tid2rc (accIs ++ [mi'], id) is

lowerSubRegVirtual stf tid2rc (accIs, id) (mi @
  MachineSingle {msOperands = [d @ MachineTemp {},
                               s1 @ MachineTemp {mtId = s1id}, sr1,
                               s2 @ MachineTemp {mtId = s2id}, sr2]} : is)
  | isMachineRegSequence mi =
    let subreg1  = toSubRegIndex sr1
        subreg2  = toSubRegIndex sr2
        operands = case (stf (tid2rc M.! s1id) subreg1,
                         stf (tid2rc M.! s2id) subreg2) of
                     ([LowSubRegIndex], [HighSubRegIndex]) -> ([d, s1, s2])
                     ([HighSubRegIndex], [LowSubRegIndex]) -> ([d, s2, s1])
        mi' = mi {msOpcode = mkMachineVirtualOpc COMBINE, msOperands = operands}
    in lowerSubRegVirtual stf tid2rc (accIs ++ [mi'], id) is

lowerSubRegVirtual stf tid2rc (accIs, id) (mi : is) =
  lowerSubRegVirtual stf tid2rc (accIs ++ [mi], id) is

lowerSubRegVirtual _ _ (is, acc) [] = (is, acc)
