{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Block Control-Flow Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.BCFG (fromFunction, isLiveIn, isLiveOut,
immediatePredecessor, immediateSuccessors, toNode, fromNode, eqvNeighborTemps,
toDot, predecessorIds, successorIds) where

import Data.String
import Data.List
import qualified Data.Set as S
import Data.Graph.Inductive
import Data.GraphViz hiding (toNode, fromNode, toDot)
import Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Attributes.HTML as H

import Common.Util

import Unison.Base hiding (toNode)
import Unison.Constructors
import Unison.Util
import Unison.Instances
import Unison.Graphs.Util
import qualified Unison.Graphs.ICFG as ICFG

fromFunction :: (BlockOperation i r -> Maybe BranchInfo) -> Function i r ->
                BCFGraph i r
fromFunction bif Function {fCode = code} =
  let nodes = map toLNode code
      edges = controlEdges bif code
  in mkGraph nodes edges

toLNode b @ Block {bLab = l} = (fromIntegral l, b)

controlEdges bif code =
    let lastBlock = bLab $ last code
    in concatMap (toEdges bif lastBlock) code

toEdges bif lastB b =
  [(toNode (bLab b), toNode s, ()) | s <- blockSucc bif lastB b]

-- | Query functions

isLiveIn b = ICFG.isLiveIn b . ICFG.fromBCFG
isLiveOut b = ICFG.isLiveOut b . ICFG.fromBCFG

-- | Gives the immediate predecessor to bId coming from pbId
immediatePredecessor bcfg bId pbId =
    let p   = esp (fromInteger pbId) (fromInteger bId) bcfg
        ipb = (case p of
                  [b'] -> b'
                  path  -> last (init path))
    in fromNode ipb

-- | Gives the immediate successors of bId
immediateSuccessors bcfg b = suc bcfg (toNode b)

label bcfg n =
    let Just l = lab bcfg n
    in l

toNode = fromIntegral
fromNode = toInteger

predecessorIds :: BCFGraph i r -> BlockId -> [BlockId]
predecessorIds bcfg id = map fromNode (pre bcfg $ toNode id)

successorIds :: BCFGraph i r -> BlockId -> [BlockId]
successorIds bcfg id = map fromNode (suc bcfg $ toNode id)

-- | Given a list of list representing a partition of temporaries, gives all
-- | pairs of temporaries (t1, t2) such that:
-- | - t1 and t2 are equivalent according to the given partition
-- | - t1 is used by the out-delimiter of block b1
-- | - t2 is defined by the in-delimiter of block b2
-- | - there is an edge b1->b2 in the control-flow graph

eqvNeighborTemps bcfg p =
    concat [sort (eqvPairs b1us b2ds p) | (b1us, b2ds) <-  edgePairs bcfg]

edgePairs :: BCFGraph i r -> [([Operand r], [Operand r])]
edgePairs g = [(outOps b1 g, inOps b2 g) | (b1, b2) <- sort (edges g)]

outOps = edgeElemsFrom blockOut
inOps = edgeElemsFrom blockIn

edgeElemsFrom f b bcfg = map edgeElem $ oAllOps (f (label bcfg b))

edgeElem t @ Temporary {} = undoPreAssign t
edgeElem MOperand {operandId = id} = mkOperandRef id
edgeElem e = e

eqvPairs s1 s2 p = [(e1, e2) | e1 <- s1, e2 <-s2, eqv e1 e2 p, e1 /= e2]
eqv e1 = any . bothMember e1
bothMember e1 e2 l = S.member e1 l && S.member e2 l

-- | Functions to print the graph as a GraphViz graph

toDot g cs simple = graphToDot (params g cs simple) g

params g cs simple =
  nonClusteredParams {
    globalAttributes =
       [GraphAttrs [if simple then toLabel ""
                    else toLabel (tuplesLabel g cs),
                    FontName graphFontName, FontSize graphFontSize]]
    , fmtNode = if simple then bcfgSimpleNodeAttributes else bcfgNodeAttributes
    , fmtEdge = if simple then const [] else bcfgEdgeAttributes g cs}

bcfgNodeAttributes (_, b) = [toLabel (bcfgNodeLabel b), shape BoxShape]

bcfgNodeLabel Block {bLab = l, bCode = code} =
  H.Table H.HTable { H.tableFontAttrs = Just [htmlFace]
                   , H.tableAttrs = [H.Border 0]
                   , H.tableRows =  headerRow l : map htmlRow code
                   }

headerRow l = H.Cells [H.LabelCell [H.Align H.HLeft] (htmlLabel l)]

htmlLabel l = H.Text [H.Str (fromString ("b" ++ show l ++ ":"))]

htmlRow (SingleOperation id i _) =
  H.Cells [
    htmlCell [H.Str (fromString (" o" ++ show id ++ ":"))]
    ,htmlCell [H.Str (fromString (show i))]
    ]

htmlFace = H.Face graphFontName

htmlCell = H.LabelCell [H.Align H.HLeft] . H.Text

bcfgSimpleNodeAttributes (_, Block {bLab = l}) =
  [toLabel ("b" ++ show l),
   shape BoxShape]

bcfgEdgeAttributes g cs (n1, n2, _) =
    let (us, ds) = (outOps n1 g, inOps n2 g)
        ecs      = congruenceTuples us ds cs
        label    = [toLabel (tuplesEdgeStr ecs)]
        dir      = [Dir Back | n1 == n2]
        attrs    = [FontSize graphFontSize, FontName graphFontName]
    in concat [label, dir, attrs]

tuplesLabel g cs =
    let ePairs = edgePairs g
        ecs    = concat [congruenceTuples us ds cs | (us, ds) <- ePairs]
        cs'    = cs \\ ecs
    in newLine ++ (if null cs' then "" else
                      "congruences:" ++ showCongruenceTuples cs')

congruenceTuples us ds cs = [(u, d) | u <- us, d <- ds, (u, d) `elem` cs]
showCongruenceTuples ts = newLine ++ showCongruences ts graphLineWidth wsWidth

tuplesEdgeStr ts = showCongruences ts 25 0

graphLineWidth = 80

graphFontSize = 12.0
