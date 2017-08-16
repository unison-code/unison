{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Useful functions on FGL graphs.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.Util
       (isLoop, isLoopEdge, pathToLEdges, inGraph, nodeLabMap, edgeLabMap,
        rootNode, removeDeeperThan, concatGraphs, edgeLab, labels,
        removeDuplicateEdges, graphFontName) where

import Data.Maybe
import Data.List
import Data.String
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Graph.Inductive

isLoop g v = elem v (suc g v)

isLoopEdge (v, w, _) = v == w

-- | Converts a path into a list of corresponding edges
pathToLEdges g [n, n'] = [labEdge g n n']
pathToLEdges g (n:n':ns) = labEdge g n n' : (pathToLEdges g (n':ns))

-- | Gives one labeled edge corresponding to the edge (n, n')
labEdge g n n' = (n, n', head $ edgeLabels g n n')

-- | Gives all labels corresponding to the edge (n, n')
edgeLabels g n n' = [l | (s, l) <- lsuc g n, s == n']

-- | Whether n is a node of graph g
inGraph g n = gelem n g

-- | Gives a map from nodes to their labels
nodeLabMap g = M.fromList $ labNodes g

-- | Gives a map from edges to their labels
edgeLabMap g = M.fromList $ [((p, c), l) | (p, c, l) <- labEdges g]

-- | Gives a node without incoming edges
rootNode g = find (\n-> indeg g n == 0) (nodes g)

-- | Removes all nodes deeper than p, assuming g is a tree
removeDeeperThan p g =
    let root = fromJust $ rootNode g
        n2l  = M.fromList $ level root g
        lim  = n2l M.! p
        dn   = filter (\n -> (n2l M.! n) > lim) $ nodes g
    in delNodes dn g

-- | Merges nodes and edges of a list of graphs
concatGraphs dgs =
  let nodes = concatMap labNodes dgs
      edges = concatMap labEdges dgs
  in mkGraph nodes edges

-- | Gives the label of a given labeled edge
edgeLab (_, _, l) = l

-- | Gives all labels of the graph
labels g = map snd $ labNodes g

-- | Remove edges with the same source, target, and label
removeDuplicateEdges g =
  let nodes = labNodes g
      edges = nub $ labEdges g
  in mkGraph nodes edges

graphFontName = fromString "Courier New" :: T.Text
