{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Algorithms and data types to analyze the resouce consumption and the latency
slack of assembly code.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.Stalls
       (BlockingResource (..), exceedsCapacity, resUsages, isActive, toMapTuple)
    where

import Unison
import qualified Data.Map as M

data BlockingResource r s =
  BlockingReg (Operand r) |
  BlockingRes s Integer deriving (Eq, Ord, Show)

exceedsCapacity cf (r, u) = (cf M.! r - u) < 0

resUsages uf cf o
    | isBarrier o = [(BlockingRes r c, 1) | (r, c) <- M.toList cf]
    | isVirtual o = []
    | otherwise =
        -- TODO: handle usages with offset different than 0
        [(BlockingRes r us, occ) |
         (Usage r us occ off) <- uf $ targetInst $ oInstructions o, off == 0]

isActive (BlockingReg _, _) = True
isActive (BlockingRes _ _, l) = l > 0

toMapTuple (BlockingRes r us, _) = (r, us)
