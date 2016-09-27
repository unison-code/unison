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
module MachineIR.Transformations.SimplifyFallthroughs
    (simplifyFallthroughs) where

import Common.Util
import MachineIR.Transformations.RemoveEmptyBlocks
import MachineIR.Transformations.DropExplicitFallthroughs

simplifyFallthroughs mf target = fixpoint (simplifyFallthroughsFor target) mf

simplifyFallthroughsFor target mf =
    let mf'  = removeEmptyBlocks mf target
        mf'' = dropExplicitFallthroughs mf' target
    in mf''
