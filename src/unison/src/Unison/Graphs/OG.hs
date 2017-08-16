{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Operand Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.OG
       (fromFunction, fromBlock, toNodeId, toDot) where

import Data.Graph.Inductive hiding (mkEdge)
import Data.GraphViz hiding (toDot)
import Data.GraphViz.Attributes.Complete

import Unison.Base
import Unison.Instances()
import Unison.Util
import Unison.Graphs.Util
import Unison.Predicates

fromFunction Function {fCode = code, fCongruences = cs} =
  let ogs = map (\b -> fromBlock b []) code
      og  = concatGraphs ogs
  in insEdges (mkCongruenceEdges cs) og

fromBlock :: Ord r => Block i r -> [CongruenceTuple r] -> OGraph i r
fromBlock Block {bCode = code} cs =
  let ops   = concatMap oAllOperands code
      bcs   = codeCongruences code cs
      nodes = map toLNode ops
      edges = mkDataFlowEdges ops code ++
              mkCopyEdges code ++
              mkNaturalEdges code ++
              mkCongruenceEdges bcs
  in mkGraph nodes edges

toLNode p = (toNodeId p, p)

toNodeId MOperand {operandId = p} = fromIntegral p
toNodeId (OperandRef p) = fromIntegral p

mkDataFlowEdges ops = concatMap (mkInstrDataFlowEdges ops)

mkInstrDataFlowEdges ops i = concatMap (mkUseEdges ops) (oDefOperands i)

mkUseEdges ops p @ MOperand {altTemps = ts} =
  let [t] = filter (not . isNullTemporary) ts
  in [mkEdge (DataFlowEdge t) p q | q <- ops, t `elem` altTemps q, p /= q]

mkCopyEdges code = map copyToLEdge (filter isCopy code)

copyToLEdge copy =
    let (s, d) = copyOps copy
    in mkEdge (OperandCopyEdge copy) s d

mkNaturalEdges code = concatMap naturalToLEdges (filter isNatural code)

naturalToLEdges i =
  [mkEdge (OperandNaturalEdge i) p q | p <- oUseOperands i, q <- oDefOperands i]

mkCongruenceEdges cs = [mkEdge OperandCongruenceEdge p q | (p, q) <- cs]

mkEdge l p q = (toNodeId p, toNodeId q, l)

-- | Functions to print the graph as a GraphViz graph
toDot :: Show r => OGraph i r -> DotGraph Node
toDot = graphToDot params

params :: Show r =>
          GraphvizParams Int (Operand r) (OGEdgeLabel i r) () (Operand r)
params = nonClusteredParams {
           globalAttributes = []
         , fmtNode = ogNodeAttributes
         , fmtEdge = ogEdgeAttributes
}

ogNodeAttributes (_, p) =
    let l = show p
    in [toLabel l, FontName graphFontName, shape Ellipse]

ogEdgeAttributes (_, _, DataFlowEdge t) =
    let l = show t
    in [toLabel l, FontName graphFontName, Style [SItem Dashed []]]

ogEdgeAttributes (_, _, OperandCopyEdge i) =
    let l = "o" ++ show (oId i)
    in [toLabel l, FontName graphFontName, Style [SItem Dotted []]]

ogEdgeAttributes (_, _, OperandNaturalEdge i) =
    let l = "o" ++ show (oId i)
    in [toLabel l, FontName graphFontName, Style [SItem Bold []]]

ogEdgeAttributes (_, _, OperandCongruenceEdge) = [Dir NoDir]
