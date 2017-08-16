{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Algorithms to estimate statically the block execution frequency.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.FrequencyEstimation (fromNaturalLoops) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.DFS

import Unison.Base

data Loop = Loop {
  loopBlocks  :: S.Set BlockId,
  loopNesting :: Integer
  } deriving Show

-- | Estimates execution frequency from the nesting level of natural loops
fromNaturalLoops :: Integer -> BCFGraph i r -> BCFGraph i r
                 -> M.Map BlockId Integer
fromNaturalLoops loopFactor cfg dt =
  let es        = edges cfg
      backEs    = filter (isBackEdge dt) es
      loop0     = Loop (S.fromList (map toInteger (nodes cfg))) (-1)
      loops'    = loop0 : map (mkLoop cfg) backEs
      loops''   = map (nest loops') loops'
      loops'''  = map decrementNest loops''
      blockNest = foldl updateBlockNest M.empty loops'''
  in M.map ((^) loopFactor) blockNest

-- | Whether the given edge is a back edge, according to the dominance tree
isBackEdge dt (p, c) = p `elem` reachable c dt

-- | Builds a loop from a given back edge n->d
mkLoop cfg (n, d) =
  let cfg' = delNode d cfg
      h    = toInteger d
      bs   = map toInteger $ reachable n (grev cfg')
  in Loop (S.fromList (h:bs)) (-1)

-- | Updates the nesting level of loop by looking at all other loops
nest loops loop =
  let n = toInteger $ length $ filter (innerLoop loop) loops
  in loop {loopNesting = n}

-- | Whether l1 is an inner loop of l2
innerLoop l1 l2 = loopBlocks l1 `S.isSubsetOf` loopBlocks l2

-- | Decrements nesting level by one
decrementNest loop @ Loop {loopNesting = n} = loop {loopNesting = n - 1}

updateBlockNest blockNest Loop {loopBlocks = bs, loopNesting = n} =
  let b2n = map (\b -> (b, n)) (S.toList bs)
  in  foldl insertIfGreater blockNest b2n

-- | Inserts (k, v) into m if v is greater than a possibly existing (k, v')
insertIfGreater m (k, v) = M.insertWith max (toInteger k) v m
