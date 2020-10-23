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
module MachineIR.Transformations.RunPreProcess (runPreProcess) where

import MachineIR.Util

import Unison.Target.API

runPreProcess mf target = runMachineTransformations (preProcess target) mf
