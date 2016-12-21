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
module MachineIR.Transformations.DropUnsupportedPseudos (dropUnsupportedPseudos) where

import MachineIR

-- Note: eventually we need to handle these pseudo-instructions

dropUnsupportedPseudos mf _target =
  filterMachineInstructions
  (not . (\mi -> isMachineEHLabel mi || isMachineCFIInstruction mi)) mf
