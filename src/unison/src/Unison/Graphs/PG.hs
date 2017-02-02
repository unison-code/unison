{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Precedence Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.PG
       (fromDependencyGraph, nonNegative, positive, mandatory, toNodeId, toCode,
        toDot) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz hiding (toDot)
import Data.GraphViz.Attributes.Complete

import Unison.Base
import Unison.Util
import Unison.Instances
import Unison.Graphs.Util
import Unison.Target.API
import Unison.Target.Query

fromDependencyGraph :: Show i => Eq i => Ord i => Ord r => Show r =>
                       OperandInfoFunction i rc -> DGraph i r -> PGraph i r
fromDependencyGraph oif dg =
  let pg   = emap (\(_, _, ls) -> precedenceType ls) dg
      code = toCode pg
      ts   = sort $ map undoPreAssign $ tUniqueOps code
      t2d  = tempMap potentialDefiner code ts
      t2us = tempMap potentialUsers code ts
      dlf  = dataLatency (tempLatencies oif)
      ptf  = dataPrecedenceType dlf
      dps  = nub (concatMap (dataPrecedences ptf t2d) code) \\ labEdges pg
      pg'  = insEdges dps pg
      rdps = nub (concatMap (revDataPrecedences ptf t2us) code) \\ labEdges pg'
      pg'' = insEdges rdps pg'
      upg  = removeDuplicateEdges pg''
  in upg

tempMap f code ts = M.fromList [(t, f t code) | t <- ts]

dataPrecedenceType dlf ts p c = precedenceType $ dlf ts p c

precedenceType ls =
  let lats = map (fromMaybe 0) ls
  in case (any (< 0) lats, all (> 0) lats) of
    (True , _    ) -> NegativePrecedence
    (_    , True ) -> PositivePrecedence
    (False, False) -> Precedence

dataPrecedences ptf t2d i =
    mapMaybe (operandPrecedence ptf t2d i) (oUseOperands i)

operandPrecedence ptf t2d i p =
  let ts = extractTemps p
  in case find isMandatory (map ((M.!) t2d) ts) of
       (Just d) -> Just (toNodeId d, toNodeId i, ptf ts d i)
       Nothing  -> Nothing

revDataPrecedences ptf t2us i =
  mapMaybe (revOperandPrecedence ptf t2us i) (oDefOperands i)

revOperandPrecedence ptf t2us i p =
  let [t] = extractTemps p
  in case filter isMandatory (t2us M.! t) of
    [u] -> Just (toNodeId i, toNodeId u, ptf [t] i u)
    _   -> Nothing

nonNegative pg = elfilter ((/=) NegativePrecedence) pg
positive pg = elfilter ((==) PositivePrecedence) pg
mandatory pg =
  let man = S.fromList $ [toNodeId i | i <- toCode pg, isMandatory i]
  in efilter (\(i, j, _) -> all ((flip S.member) man) [i, j]) pg

toNodeId = fromIntegral . oId

toCode = map snd . labNodes

-- | Functions to print the graph as a GraphViz graph
toDot :: (Eq i, Show i) => PGraph i r -> DotGraph Node
toDot = graphToDot params

params :: (Eq i, Show i) => GraphvizParams n (BlockOperation i r)
          PrecedenceType () (BlockOperation i r)
params = nonClusteredParams {
           globalAttributes = []
         , fmtNode = pgNodeAttributes
         , fmtEdge = pgEdgeAttributes
}

pgNodeAttributes (_, i) =
  let l = "o" ++ show (oId i) ++ ": " ++ showInstrs (oOpr i)
      c = X11Color (if isMandatory i then Black else copyColor)
  in [toLabel l, FontName graphFontName, FontColor c, shape BoxShape]

pgEdgeAttributes (_, _, PositivePrecedence) = [Style [SItem Bold []]]
pgEdgeAttributes (_, _, Precedence)         = []
pgEdgeAttributes (_, _, NegativePrecedence) = [Style [SItem Dotted []]]

copyColor = Gray40
