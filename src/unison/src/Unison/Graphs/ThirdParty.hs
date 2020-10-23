{-|
Maintainer  :  rcas@acm.org

Graph algorithms extracted from third-party components.

-}
module Unison.Graphs.ThirdParty (componentsOf, cyclesIn') where

import Data.Graph.Inductive.Graph

import Data.List
import Control.Arrow
import Data.Function(on)

-- Algorithms extracted from Graphalyze-0.14.1.0

{-

Copyright (c) 2008, Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

-}

-- | Find all connected components of a graph.
componentsOf :: (DynGraph g) => g a b -> [g a b]
componentsOf = unfoldr splitComponent

-- | Find the next component and split it off from the graph.
splitComponent :: (DynGraph g) => g a b -> Maybe (g a b, g a b)
splitComponent g
    | isEmpty g = Nothing
    | otherwise = Just .          -- Get the type right
                  first buildGr . -- Create the subgraph
                  extractNode .   -- Extract components of subgraph
                  first Just .    -- Getting the types right
                  matchAny $ g    -- Choose an arbitrary node to begin with

-- | Extract the given node and all nodes it is transitively
--   connected to from the graph.
extractNode :: (DynGraph g) => Decomp g a b -> ([Context a b], g a b)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | isEmpty gr = ([ctxt], empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
      nbrs = neighbors' ctxt

-- | Helper function for 'extractNode' above.
nodeExtractor :: (DynGraph g) => ([Context a b], g a b) -> Node
              -> ([Context a b], g a b)
nodeExtractor cg@(cs,g) n
    | gelem n g = first (++ cs) . extractNode $ match n g
    | otherwise = cg

{- $cycles
   Cycle detection.  Find cycles by finding all paths from a given
   node, and seeing if it reaches itself again.
 -}

-- | Find all cycles in the given graph, returning just the nodes.
cyclesIn' :: (DynGraph g) => g a b -> [[Node]]
cyclesIn' = concat . unfoldr findCycles . mkSimple

-- | Find all cycles containing a chosen node.
findCycles :: (DynGraph g) => g a b -> Maybe ([[Node]], g a b)
findCycles g
    | isEmpty g = Nothing
    | otherwise = Just . getCycles . matchAny $ g
    where
      getCycles (ctx,g') = (cyclesFor (ctx, g'), g')

-- | Find all cycles for the given node.
cyclesFor :: (DynGraph g) => GDecomp g a b -> [[Node]]
cyclesFor = map init .
            filter isCycle .
            pathTree .
            first Just
    where
      isCycle p = not (single p) && (head p == last p)


-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree             :: (DynGraph g) => Decomp g a b -> [[Node]]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = node' ct
      sucs = suc' ct
      -- Avoid infinite loops by not letting it continue any further
      ct' = makeLeaf ct
      g' = ct' & g
      subPathTree gr n' = pathTree $ match n' gr

-- | Remove all outgoing edges
makeLeaf           :: Context a b -> Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where
      -- Ensure there isn't an edge (n,n)
      p' = filter (\(_,n') -> n' /= n) p

-- | Makes the graph a simple one, by removing all duplicate edges and loops.
--   The edges removed if duplicates exist are arbitrary.
mkSimple :: (DynGraph gr) => gr a b -> gr a b
mkSimple = gmap simplify
    where
      rmLoops n = filter ((/=) n . snd)
      rmDups = nubBy ((==) `on` snd)
      simpleEdges n = rmDups . rmLoops n
      simplify (p,n,l,s) = (p',n,l,s')
          where
            p' = simpleEdges n p
            s' = simpleEdges n s

-- | Return true if and only if the list contains a single element.
single     :: [a] -> Bool
single [_] = True
single  _  = False
