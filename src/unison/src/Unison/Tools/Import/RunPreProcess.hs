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
module Unison.Tools.Import.RunPreProcess (runPreProcess) where

import MachineIR.Util

import Unison.Target.API

runPreProcess mf target = runMachineTransformations (preProcess target) mf