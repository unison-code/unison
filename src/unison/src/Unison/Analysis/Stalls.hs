{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Algorithms and data types to analyze the resouce consumption and the latency
slack of assembly code.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.Stalls
       (BlockingResource (..), BlockingResourceState (..),
        stepCycle, exceedsCapacity, resUsages, isActive, toMapTuple)
    where

import Unison
import qualified Data.Map as M

data BlockingResource r s =
  BlockingReg (Operand r) |
  BlockingRes s Integer deriving (Eq, Ord, Show)

data BlockingResourceState r s =
  BlockingResourceState {
    brsResource   :: BlockingResource r s,
    brsOccupation :: Integer,
    brsOffset     :: Integer
    } deriving (Eq, Ord, Show)

stepCycle brs @ BlockingResourceState {brsOccupation = occ, brsOffset = off}
  | off > 0  = brs {brsOffset = off - 1}
  | off == 0 = brs {brsOccupation = occ - 1}

exceedsCapacity cf (r, u) = (cf M.! r - u) < 0

resUsages uf cf o
    | isBarrier o = [BlockingResourceState (BlockingRes r c) 1 0
                      | (r, c) <- M.toList cf]
    | isVirtual o = []
    | otherwise =
        -- TODO: handle usages with offset less than 0
        [(BlockingResourceState (BlockingRes r us) occ off) |
         (Usage r us occ off) <- uf $ targetInst $ oInstructions o, off >= 0]

isActive BlockingResourceState {brsResource = BlockingReg {}} = True
isActive BlockingResourceState {brsResource = BlockingRes {},
                                brsOccupation = occ} = occ > 0

toMapTuple BlockingResourceState {brsResource = BlockingRes r us} = (r, us)
