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
module MachineIR.Transformations.SimplifyFallthroughs
    (simplifyFallthroughs) where

import Common.Util
import MachineIR.Transformations.RemoveEmptyBlocks
import MachineIR.Transformations.DropExplicitFallthroughs
import MachineIR.Transformations.MergeBlocks

simplifyFallthroughs onlySplits mf target =
  fixpoint (simplifyFallthroughsFor onlySplits target) mf

simplifyFallthroughsFor onlySplits target mf =
    let mf1 = removeEmptyBlocks mf target
        mf2 = if onlySplits then mf1 else dropExplicitFallthroughs mf1 target
        mf3 = mergeBlocks onlySplits mf2 target
    in mf3
