{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Coalescing Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.CG (fromFunction, fromBlock, toNodeId, temps, toDot) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph.Inductive hiding (mkEdges)
import Data.GraphViz hiding (toDot)
import Data.GraphViz.Attributes.Complete

import Unison.Base
import Unison.Instances()
import Unison.Util
import Unison.Predicates
import Unison.Constructors
import Unison.Graphs.Util

fromFunction :: Ord r => Function i r -> CGraph i r
fromFunction Function {fCode = code, fCongruences = cs} =
  let fCode = flatten code
      bts   = boundaryTemporaries fCode
      pas   = M.fromList [(t, r) | (Temporary t (Just r)) <- tOps fCode]
      nodes = nub $ concatMap (bbToLNodes bts pas) code
      edges = mkCGEdges bts fCode cs
  in mkGraph nodes edges

fromBlock :: Ord r => Block i r -> [CongruenceTuple r] -> CGraph i r
fromBlock b @ Block {bCode = code} cs =
  let ts  = S.fromList $ tUniqueOps code
      bcs = [(t, t') | (t, t') <- cs, S.member t ts && S.member t' ts]
  in fromFunction (mkFunction [b] bcs [] [] [] 0 0 [] ("", []) [] [] "")

boundaryTemporaries :: Ord r => [BlockOperation i r] -> S.Set (Operand r)
boundaryTemporaries = S.fromList . tUniqueOps . filter isDelimiter

bbToLNodes bts pas Block {bCode = code} = map (toLNode bts pas) (tUniqueOps code)

toLNode bts pas t = (toNodeId t, (S.member t bts, M.lookup (tId t) pas))

mkCGEdges bts fCode cs =
    let copies = filter isCopy fCode
        opcs   = [(findOperand fCode p, findOperand fCode q) | (p, q) <- cs]
        comps  = filter isComponent fCode
        phis   = filter isPhi fCode
    in concatMap mkLEdgeFromCopy copies ++
       concatMap (mkLEdgesFromSGes bts) opcs ++
       concatMap mkLEdges comps ++
       concatMap mkLEdges phis

mkLEdgeFromCopy copy =
    let (s, d) = copyOps copy
    in mkEdges (CopyEdge copy) s d

mkLEdgesFromSGes bts (ts, ts') =
  combineTemps (toCEdge bts) ts ts' ++ combineTemps (toCEdge bts) ts' ts

toCEdge bts t t' =
  (toNodeId t, toNodeId t', CongruenceEdge (S.member t bts && S.member t' bts))

mkLEdges bi @ SingleOperation
    {oOpr = Virtual Combine {oCombineLowU = lu, oCombineHighU = hu,
                              oCombineD = d}} =
      mkEdges (CombineEdge bi) lu d ++ mkEdges (CombineEdge bi) hu d

mkLEdges bi @ SingleOperation {oOpr = Virtual Low {oLowU = u, oLowD = d}} =
    mkEdges (LowEdge bi) u d

mkLEdges bi @ SingleOperation {oOpr = Virtual High {oHighU = u, oHighD = d}} =
    mkEdges (HighEdge bi) u d

mkLEdges bi @ SingleOperation {
  oOpr = Virtual Split2 {oSplit2U = u, oSplit2LowD = ld, oSplit2HighD = hd}} =
    concatMap (mkEdges (SplitEdge bi) u) [ld, hd]

mkLEdges bi @ SingleOperation {
  oOpr = Virtual Split4 {oSplit4U = u, oSplit4LowLowD = lld,
                         oSplit4LowHighD = lhd, oSplit4HighLowD = hld,
                         oSplit4HighHighD = hhd}} =
    concatMap (mkEdges (SplitEdge bi) u) [lld, lhd, hld, hhd]

mkLEdges po @ SingleOperation {oOpr = Virtual Phi {oPhiD = d}} =
    let us' = map fst (phiUses po)
    in concat [mkEdges (CopyEdge po) u d | u <- us']

findOperand _ t @ Temporary {} = t
findOperand code (OperandRef p) =
  let i  = fromJust $ find (refersTo p) code
      tc = fromJust $ find (isChoiceId p) (oAllOps i)
  in tc

isChoiceId p o = isMOperand o && operandId o == p

refersTo p i = any (isChoiceId p) (oAllOps i)

mkEdges e = combineTemps (\t t' -> (toNodeId t, toNodeId t', e))

combineTemps f ts ts' = [f t t' | t <- extractTemps ts, t' <- extractTemps ts']

toNodeId Temporary {tId = tId} = fromIntegral tId

temps cg = [Temporary (toInteger n) r | (n, (_, r)) <- labNodes cg]

-- | Functions to print the graph as a GraphViz graph
toDot :: Show i => Show r => CGraph i r -> DotGraph Node
toDot = graphToDot params

params :: Show r =>
          GraphvizParams Int
          (Bool, Maybe (Operand r))
          (CGEdgeLabel i r)
          ()
          (Bool, Maybe (Operand r))
params = nonClusteredParams {
           globalAttributes = []
         , fmtNode = cgNodeAttributes
         , fmtEdge = cgEdgeAttributes
}

cgNodeAttributes (t, (_, r)) =
    let l = show (mkCompleteTemp t r)
    in [toLabel l, FontName graphFontName, shape Ellipse,
        Style [SItem Filled []]]

cgEdgeAttributes (_, _, CopyEdge i) =
    let l = "o" ++ show (oId i)
    in [toLabel l, FontName graphFontName, Style [SItem Dashed []]]

cgEdgeAttributes (_, _, CongruenceEdge _) = []

cgEdgeAttributes (_, _, CombineEdge i) =
    [toLabel $ "combine (o" ++ show (oId i) ++ ")", FontName graphFontName,
     Style [SItem Dotted []]]

cgEdgeAttributes (_, _, LowEdge i) =
    [toLabel $ "low (o" ++ show (oId i) ++ ")", FontName graphFontName,
     Style [SItem Dotted []]]

cgEdgeAttributes (_, _, HighEdge i) =
    [toLabel $ "high (o" ++ show (oId i) ++ ")", FontName graphFontName,
     Style [SItem Dotted []]]

cgEdgeAttributes (_, _, SplitEdge i) =
    [toLabel $ "split (o" ++ show (oId i) ++ ")", FontName graphFontName,
     Style [SItem Dotted []]]
