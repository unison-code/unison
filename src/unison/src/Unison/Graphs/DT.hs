{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Dominance Tree.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.DT (fromCFG, dominators) where

import Data.Graph.Inductive

fromCFG :: Gr a b -> Gr a b
fromCFG cfg =
    let nodes = labNodes cfg
        edges = if noNodes cfg > 0
                then [(p, c, edge cfg p c) | (c, p) <- iDom cfg 0]
                else []
    in  mkGraph nodes edges

label g n =
    let Just l = lab g n
    in l

dominators dt n =
    let ds = reaching n dt
    in map (label dt) ds

reaching n = reachable n . grev

edge g n1 n2 =
    let [l] = [l | (n1', n2', l) <- labEdges g, (n1, n2) == (n1', n2')]
    in l
