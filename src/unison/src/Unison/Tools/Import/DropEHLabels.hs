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
module Unison.Tools.Import.DropEHLabels (dropEHLabels) where

import MachineIR

-- Note that this pass is temporary, eventually we need to handle these
-- pseudo-instructions

dropEHLabels mf _target = filterMachineInstructions (not . isMachineEHLabel) mf
