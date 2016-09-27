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
module Unison.Tools.Export.RunPostProcess (runPostProcess) where

import MachineIR.Util

import Unison.Target.API

runPostProcess mf target = runMachineTransformations (postProcess target) mf