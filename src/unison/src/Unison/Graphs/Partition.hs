{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Data structure to represent a set partition.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.Partition
       (fromPairList, fromNodes, toPairList, complete, toList, fromGraph,
        toGraph, fromPartitionMap, toPartitionMap, equivalent,
        equivalentPartition, addElement, delElement, connectElements,
        disconnectElements, size, toDot) where

import Data.Ord
import Data.List
import qualified Data.Map as M
import Data.Graph.Inductive hiding (fromGraph, size)
import Data.GraphViz hiding (toNode, fromNode, toDot)
import Data.GraphViz.Attributes.Complete

import Unison.Base
import Unison.Instances
import Unison.Graphs.Util
import Unison.Graphs.ThirdParty

instance Eq a => Eq (Partition a) where
    p1 == p2 = toList p1 == toList p2

instance Show a => Show (Partition a) where
    show = show . toList

fromPairList tuples =
    let nodes = tupleToLNodes tuples
        edges = [(toNode e1, toNode e2, ()) | (e1, e2) <- tuples]
    in Partition (mkGraph nodes edges)

fromNodes nodes = Partition (mkGraph (zip (map toNode nodes) (repeat ())) [])

tupleToLNodes tuples =
    let (firstElements, secondElements) = unzip tuples
    in zip (nub (map toNode (firstElements ++ secondElements))) (repeat ())

toPairList = edges . graph

complete p elements =
    let g              = toGraph p
        singleElements = filter (not . inGraph g) (map toNode elements)
        g'             = insNodes (zip singleElements (repeat ())) g
    in p {graph = g'}

toList = components . toGraph

fromGraph = Partition . emap (const ()) . nmap (const ())
toGraph = graph

fromPartitionMap e2pId =
  let e2pId' = sortBy (comparing snd) $ M.toList e2pId
      ps     = map (map fst) $ groupBy (equaling snd) e2pId'
      tuples = concatMap listToPairs ps
  in fromPairList tuples

listToPairs []    = []
listToPairs (e:l) = zip (e:l) l

toPartitionMap p =
  let ip    = zip [0..] (toList p)
      e2pId = M.fromList $ concat [zip e (repeat pId) | (pId, e) <- ip]
  in e2pId

equivalent p e = find (\pl -> (toNode e) `elem` pl) (toList p)

equivalentPartition p e =
    case find (\pc -> (toNode e) `elem` nodes pc) (componentsOf (graph p)) of
      Just g  -> Just (Partition g)
      Nothing -> Nothing

addElement :: Partitionable a => Partition a -> a -> Partition a
addElement p e = graphMap (insNode (toNode e, ())) p

delElement p e = graphMap (delNode (toNode e)) p

connectElements p (e, e') = graphMap (insEdge (toNode e, toNode e', ())) p

disconnectElements p (e, e') = graphMap (delEdge (toNode e, toNode e')) p

size (Partition p) = noNodes p

graphMap f p = p {graph = f $ toGraph p}

-- | Functions to print the graph as a GraphViz graph

toDot :: Show a => (Node -> a) -> Partition a -> DotGraph Node
toDot f p = graphToDot (params f) (graph p)

params f = nonClusteredParams {
             isDirected       = False
           , globalAttributes = []
           , fmtNode          = cgNodeAttributes f
           , fmtEdge          = const []
           }

cgNodeAttributes f (t, ()) =
    let l = show (f t)
    in [toLabel l, FontName graphFontName]
