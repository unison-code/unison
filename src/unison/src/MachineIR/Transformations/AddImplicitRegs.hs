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
module MachineIR.Transformations.AddImplicitRegs (addImplicitRegs) where

import MachineIR
import Unison
import Unison.Target.API

addImplicitRegs mf target =
  let rwif = readWriteInfo target
  in mapToMachineInstruction (addImplicitRegsToInstr rwif) mf

addImplicitRegsToInstr rwif mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                                msOperands = mos} =
  let rws   = rwif (TargetInstruction i)
      imp   = [MachineReg u [mkMachineRegImplicit] |
               (OtherSideEffect u) <- fst rws ++ snd rws] ++
              [MachineReg d [mkMachineRegImplicitDefine] |
               (OtherSideEffect d) <- snd rws]
      mos'  = mos ++ imp
  in mi {msOperands = mos'}
