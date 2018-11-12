{-|
Copyright   :  Copyright (c) 2018, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Transformations.ReorderInstructions (reorderInstructions) where

import Data.List
import Data.Ord

import Unison.Base
import Unison.Util
import Unison.Target.Query

-- This pass is needed since 'minizinc-solver' assumes that the indices of the
-- instructions in an operation are increasing.

reorderInstructions f @ Function {fCode = code} _target =
    let im    = instructionManager $ flatten code
        code' = mapToOperationInBlocks (mapToInstructions (reoderIns im)) code
    in f {fCode = code'}

reoderIns im is = sortBy (comparing (ioId . toIndexedInstruction im)) is
