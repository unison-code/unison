{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Operation Control-Flow Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE FlexibleContexts #-}
module Unison.Graphs.ICFG
    (fromBCFG, isLiveIn, isLiveOut, bIn, bOut, nodeInstr, nodeBlock, toNode,
    toDot) where

import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz hiding (toNode, toDot)
import Data.GraphViz.Attributes.Complete

import Unison.Base hiding (Attributes, toNode)
import Unison.Predicates
import Unison.Util
import Unison.Graphs.Util

fromBCFG bcfg | not (any isIn (flattenBcfg bcfg)) = mkEmpty
fromBCFG bcfg =
  let nodes   = extractNodes bcfg
      edges   = icfgEdges bcfg nodes ++ straightEdges nodes
  in mkGraph nodes edges

flattenBcfg :: BCFGraph i r -> [BlockOperation i r]
flattenBcfg = flatten . extractBlocks

mkEmpty = mkGraph [] [] :: ICFGraph i r

extractBlocks bcfg = map (label bcfg) (nodes bcfg)

extractNodes = concatMap blockNodes .  extractBlocks

blockNodes Block {bLab = l, bCode = code} =
  [(fromInteger (oId i), (l, i)) | i <- code]

icfgEdges bcfg nodes =
    [(toOut p nodes, toIn c nodes, ()) | (p, c) <- edges bcfg]

bOut icfg b = toOut b (labNodes icfg)
bIn icfg b = toIn b (labNodes icfg)

toIn = toOperation isIn
toOut = toOperation isOut

toOperation f b nodes =
    let [s'] = [s | (s, (b', i)) <- nodes, b' == toInteger b, f i]
    in s'

straightEdges ((i, (b, _)) : (j, (b', jInst)) : nodes) | b == b' =
    (i, j, ()) : straightEdges ((j, (b', jInst)) : nodes)
straightEdges (_ : nodes) = straightEdges nodes
straightEdges [] = []

isLiveIn  = isLiveAtEdge bIn
isLiveOut = isLiveAtEdge bOut

-- | a temporary t is live[-in/-out] of a block b iff there exists a
-- | path P from e to a user of t such that P does not contain d
-- |
-- | where:
-- | - e is the [in-delimiter (live-in) / out-delimiter (live-out)] of b
-- | - d is the instruction that defines t
-- |
-- | This naive liveness test is extracted from the ideas in Section 2.3 in
-- | [Boissinot et al. 2006: "Fast liveness checking for SSA-form programs"].
-- | This paper proposes a much faster algorithm that could be implemented if
-- | necessary!

isLiveAtEdge ::
    (ICFGraph i r -> Int -> Int) -> Int -> ICFGraph i r -> Operand r -> Bool
isLiveAtEdge toEdgeInstr b icfg t =
    let e       = toEdgeInstr icfg b
        [d]     = [i | (i, (_, oOper)) <- labNodes icfg, isDefiner t oOper]
        icfg'   = delNode d icfg
        rs      = reachable e icfg'
        lrs     = map (nodeInstr icfg') rs
        in any (isUser t) lrs

label icfg n = fromJust $ lab icfg n

nodeBlock icfg n = fst $ label icfg n
nodeInstr icfg n = snd $ label icfg n

toNode n = fromInteger $ oId n

-- | Functions to print the graph as a GraphViz graph
toDot :: Show (Operation i r) => ICFGraph i r -> DotGraph Node
toDot = graphToDot params

params :: Show (Operation i r) =>
          GraphvizParams Int (ICFGLabel i r) el () (ICFGLabel i r)
params = nonClusteredParams {
           globalAttributes = []
         , fmtNode = icfgNodeAttributes
         , fmtEdge = const []
}

icfgNodeAttributes :: Show (Operation i r) => (Int, (ICFGLabel i r)) ->
                      Attributes
icfgNodeAttributes (_, (_, SingleOperation {oId = id, oOpr = i})) =
    let l = "o" ++ show id ++ ": " ++ show i
    in  [toLabel l, shape BoxShape, FontName graphFontName]
