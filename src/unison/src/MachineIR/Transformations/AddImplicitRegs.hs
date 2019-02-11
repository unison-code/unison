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
module MachineIR.Transformations.AddImplicitRegs (addImplicitRegs) where

import Data.List
import Data.Maybe

import MachineIR
import Unison
import Unison.Target.API

addImplicitRegs mf target =
  let rwif = readWriteInfo target
      oif  = operandInfo target
      mf1  = mapToMachineInstruction (addImplicitRegsToInstr rwif) mf
      mf2  = mapToMachineBlock (addImplicitRegsToBundles oif) mf1
  in mf2

addImplicitRegsToInstr rwif mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                                msOperands = mos} =
  let rws   = rwif (TargetInstruction i)
      imp   = [MachineReg u [mkMachineRegImplicit] |
               (OtherSideEffect u) <- fst rws ++ snd rws] ++
              [MachineReg d [mkMachineRegImplicitDefine] |
               (OtherSideEffect d) <- snd rws]
      mos'  = mos ++ imp
  in mi {msOperands = mos'}

addImplicitRegsToBundles oif mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = map (addImplicitRegsToBundle oif) mis}

addImplicitRegsToBundle oif mb @ MachineBundle {mbInstrs = mis} =
  let mos = nub $ concatMap (extractImplicitRegs oif) mis
  in mb {mbOperands = mos}
addImplicitRegsToBundle _ mi = mi

extractImplicitRegs oif MachineSingle {msOpcode = MachineTargetOpc i,
                                       msOperands = mos} =
  let (ds, us) = splitAt (length $ snd $ oif i) mos
      ds'      = mapMaybe (takeImplicit mkMachineRegImplicitDefine) ds
      -- already implicit uses and defines are included in 'us' and not modified
      -- by 'takeImplicit':
      us'      = mapMaybe (takeImplicit mkMachineRegImplicit) us
  in ds' ++ us'

takeImplicit mif mr @ MachineReg {mrFlags = []} = Just mr {mrFlags = [mif]}
-- pass through implicit operands
takeImplicit _   mr @ MachineReg {} = Just mr
takeImplicit _ _ = Nothing
