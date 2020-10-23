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
module MachineIR.Transformations.DropUnsupportedPseudos (dropUnsupportedPseudos) where

import MachineIR

-- Note: eventually we need to handle these pseudo-instructions

dropUnsupportedPseudos mf _target =
  filterMachineInstructions
  (not . (\mi -> isMachineEHLabel mi || isMachineCFIInstruction mi)) mf
