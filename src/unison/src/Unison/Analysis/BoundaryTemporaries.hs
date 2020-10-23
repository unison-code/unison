{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Algorithms to compute copy-related pairs of boundary temporaries.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.BoundaryTemporaries (boundaryPairs) where

import Data.List
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Query.DFS

import Unison
import Unison.Graphs.Util
import Unison.Graphs.ThirdParty

boundaryPairs cg =
  let cg' = elfilter isCopyOrLocal cg
      cs  = componentsOf cg'
      cs' = filter severalBoundaryTemps cs
      ccs = concatMap compBoundaryPairs cs'
  in sort ccs

isCopyOrLocal (CopyEdge _)       = True
isCopyOrLocal (CongruenceEdge b) = not b
isCopyOrLocal _                  = False

severalBoundaryTemps cg = length (boundaryTemps cg) > 1

isBoundaryTemp (_, (b, _)) = b

boundaryTemps cg = filter isBoundaryTemp (labNodes cg)

compBoundaryPairs cg =
  let bts = [t | (t, _) <- boundaryTemps cg]
      ps  = [(ti, tj) | ti <- bts, tj <- bts,
             ti < tj, -- Assumes that temporary indexes are normalized
             existsLocalPath cg ti tj,
             not (areNeighbors cg ti tj)]
      pcs = [((mkTemp ti, mkTemp tj), pathCopies cg ti tj) | (ti, tj) <- ps]
  in pcs

existsLocalPath _ i i' | i == i' = False
existsLocalPath cg i j =
  let otherBs = [n | (n, (b, _)) <- labNodes cg, b, n /= i, n /= j]
      cg'     = delNodes otherBs cg
  in j `elem` reachable i cg'

areNeighbors g i j = j `elem` neighbors g i

pathCopies cg ti tj =
  let p  = esp ti tj cg
      es = pathToLEdges cg p
      cs = mapMaybe edgeToCopy es
  in cs

edgeToCopy (_, _, CopyEdge i) = Just i
edgeToCopy (_, _, CongruenceEdge False) = Nothing
