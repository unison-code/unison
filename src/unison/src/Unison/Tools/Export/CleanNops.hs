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
module Unison.Tools.Export.CleanNops (cleanNops) where

import MachineIR

import Unison
import Unison.Target.API

cleanNops f target =
  let (Natural nf) = nop target
      n = oTargetInstr $ fromSingleton $ oIs nf
  in mapToMachineBlock (cleanBlockNops n) f

cleanBlockNops n mi @ MachineBlock {mbInstructions = mis} =
    let mis' = map (cleanBundleNops n) mis
    in mi {mbInstructions = mis'}

cleanBundleNops n MachineBundle {
  mbInstrs = [mi @ MachineSingle {msOpcode = MachineTargetOpc n'}]}
    | n == n' = mi
-- Headless bundles model branches with delay slots and should not be cleaned.
cleanBundleNops _ mb @ MachineBundle {mbHead = False} = mb
cleanBundleNops n mb @ MachineBundle {mbInstrs = mis} =
    case filter (not . hasTargetOpc n) mis of
      [mi] -> mi
      mis' -> mb {mbInstrs = mis'}
cleanBundleNops _ mi = mi

hasTargetOpc n MachineSingle {msOpcode = MachineTargetOpc n'} = n == n'
hasTargetOpc _ _ = False
