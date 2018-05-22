{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Dependency Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.DG
       (fromFunction, fromBlock, dependencies, precede, inDistances, toNodeId,
        toDot) where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz hiding (toDot)
import Data.GraphViz.Attributes.Complete
import Text.PrettyPrint hiding (Style)

import Common.Util

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Instances
import Unison.Graphs.Util
import Unison.Graphs.ThirdParty
import Unison.Target.API
import Unison.Target.Query

fromFunction rwlf rm oif Function {fCode = code} =
  let dgs = map (fromBlock rwlf rm oif) code
  in concatGraphs dgs

fromBlock :: Show i => Eq i => Ord i => Show r => Ord r =>
             ReadWriteLatencyFunction i r -> ResourceManager i s ->
             OperandInfoFunction i rc -> Block i r -> DGraph i r
fromBlock rwlf rm oif Block {bCode = code} =
  let lfs      = latencies rm
      t2ls     = tempLatencies oif
      dg       = mkGraph (map toLNode code) []
      edg      = foldl (insertEdges code) dg
                  [
                   dataEdges t2ls,
                   readWriteEdges rwlf,
                   -- TODO: boundary edges are just a type of "readWrite" edges,
                   -- that would also possibly make the extended edges
                   -- unnecessary
                   boundaryEdges lfs,
                   extendedEdges lfs,
                   callFunctionEdges
                  ]
  in edg

insertEdges code dg f =
    let newEdges = f dg code
    in insEdges newEdges dg

toLNode i = (toNodeId i, i)

dataEdges t2ls _ code =
    let dts   = [(d, oDefs d) | d <- code]
        deps  = concat [[(d, u, [t]) | t <- ts, u <- code, isUser t u]
                       | (d, ts) <- dts]
        deps' = concentrate deps
    in map (toDataEdge t2ls) deps'

concentrate deps =
  let dm = M.fromListWith (++) [((d, u), [t]) | (d, u, [t]) <- deps]
  in [(d, u, ts) | ((d, u), ts) <- M.toList dm]

readWriteEdges rwlf _ code =
    let code' = map cleanRedundantReads code
        rwos  = nub (concatMap oRWObjects code')
        deps  = concatMap (readWriteEdgesForObject code') rwos
    in map (toReadWriteEdge rwlf) deps

readWriteEdgesForObject code rwo =
    let rwis    = filter (\o -> isReadOf rwo o || isWriteOf rwo o) code
        chunks  = splitKeepDelimsLR (isRWBarrier rwo) rwis
        chunks' = filter (\l -> length l > 1) chunks
        deps    = concatMap (addReadWriteEdges rwo) chunks'
    in deps

isRWBarrier rwo o = isWriteOf rwo o && isMandatory o

splitKeepDelimsLR f l =
    let chunksR = split (keepDelimsR $ whenElt f) l
        chunksL = split (keepDelimsL $ whenElt f) l
        chunks  = [sort $ nub $ cl ++ cr | (cl, cr) <- zip chunksL chunksR]
    in chunks

addReadWriteEdges rwo chunk =
    [(o1, o2, rwo, (fromJust $ accessType rwo o1, fromJust $ accessType rwo o2))
     | o1 <- chunk, o2 <- chunk, oId o1 < oId o2, any (isWriteOf rwo) [o1,o2]]

boundaryEdges lfs dg code =
    let Just vIn  = find isIn code
        Just vOut = find isOut code
        orphans   = [o | o <- code, isOrphan dg (toNodeId o), o /= vIn]
        widows    = [w | w <- code, isWidow dg (toNodeId w), w /= vOut]
        deps      = [(vIn, o) | o <- orphans] ++ [(w, vOut) | w <- widows]
                    ++ [(vIn, vOut)]
    in map (toControlEdge lfs) deps

extendedEdges lfs dg code =
  let mis  = S.fromList (mandatoryIds code)
      dg'  = efilter (not . anyIn mis) dg
      deps = nub $ concatMap (extendedDependencies dg) (componentsOf dg')
  in map (toExtendedEdge lfs) deps

-- | Note (for potential optimization): cg is a tree
extendedDependencies _ cg
  | noNodes cg == 1 && (isMandatory $ snd $ fromSingleton $ labNodes cg) = []
extendedDependencies dg cg =
  let cns  = S.fromList $ nodes cg
      ns   = S.fromList $ concatMap (neighbors dg) (S.toList cns)
      ms   = S.difference ns cns
      mns  = [(m, toIstr dg m) | m <- S.toList ms]
      es   = filter (\e -> isCross ms cns e) (labEdges dg)
      cg'  = insNodes mns cg
      cg'' = insEdges es cg'
      tg   = efilter (not . isLoopEdge) (trc cg'')
      n2i  = nodeLabMap dg
      e2l  = edgeLabMap dg
      deps = [(n2i M.! p, n2i M.! c) | (p, c) <- edges tg,
                                        not (subsumedEdge e2l (p, c))]
  in deps

anyIn is (p, c, _) = S.member p is || S.member c is

isCross ns1 ns2 (p, c, _) =
  (S.member p ns1 && S.member c ns2) || (S.member p ns2 && S.member c ns1)

subsumedEdge e2l e =
    case M.lookup e e2l of
      Nothing  -> False
      (Just l) -> subsumedExtendedDep l

subsumedExtendedDep (ReadWriteDependency _ (Read, Write), _, _) = False
subsumedExtendedDep _ = True

callFunctionEdges _ code =
    let funs = filter isFun code
        deps = concatMap (\f -> [(callOf f code, f), (f, callOf f code)]) funs
    in map (toCallFunctionEdge) deps

mandatoryIds code = [toNodeId i | i <- code, isMandatory i]

toDataEdge t2ls (p, c, ts) =
  mkLEdge (dataLatency t2ls ts) (DataDependency ts) (p, c)
toReadWriteEdge rwlf (p, c, rwo, a) =
  mkLEdge (readWriteLat rwlf rwo a) (ReadWriteDependency rwo a) (p, c)
toControlEdge lfs (p, c) = mkLEdge (controlLatency lfs) ControlDependency (p, c)
toExtendedEdge lfs (p, c) =
  mkLEdge (controlLatency lfs) ExtendedDependency (p, c)
toCallFunctionEdge (p, c) = mkLEdge callFunctionLatency ControlDependency (p, c)

readWriteLat rwlf rwo (pa, ca) p c =
  [maximum [rwlf rwo (pi, pa) (ci, ca) | ci <- oAnnInstructions c]
           | pi <- oAnnInstructions p]

controlLatency lfs p c
    | isIn p     = lfs p
    | otherwise  = [maybeEval (-) pl cl | pl <- lfs p] where cl = head (lfs c)

callFunctionLatency p c
    | (isCall p || isTailCall p) && isFun c = [Just 1]
    | isFun p && (isCall c || isTailCall c) = [Just (-1)]

mkLEdge lfs t (p, c) =
    let m = isMandatory p && isMandatory c
        l = lfs p c
    in (toNodeId p, toNodeId c, (t, m, l))

-- | Whether i precede j in dg
precede _ i i' | i == i' = True
precede dg i j =
    case esp (toNodeId i) (toNodeId j) dg of
      [] -> False
      _  -> True

inDistances dg =
    let inn = fromJust $ rootNode dg
        cg  = emap (const (-1)) dg
    in map (inDistance cg inn) (nodes dg)

inDistance cg inn n = (n, length $ justSp inn n cg)

justSp s d g =
    case sp s d g of
      Just p  -> p
      Nothing -> error ("justSp: did not find any path")

toNodeId = fromIntegral . oId

toIstr dg = fromJust . lab dg

isOrphan g = noFixedDeps . inn g
isWidow g = noFixedDeps . out g

noFixedDeps = null . filter (\(_, _, (d, _, _)) -> isFixedDependency d)

dependencies dg =
    nub $ sort [(toInteger p, toInteger c, ls)
                    | (p, c, (d, _, ls)) <- labEdges dg, isFixedDependency d]

isFixedDependency ControlDependency         = True
isFixedDependency ExtendedDependency        = True
isFixedDependency (ReadWriteDependency _ _) = True
isFixedDependency _                         = False

-- | Functions to print the graph as a GraphViz graph
toDot :: (Eq i, Show i, Ord r, Show r) => DGraph i r -> Bool -> DotGraph Node
toDot g clearExtDeps =
    let g'  = if clearExtDeps then elfilter (not . isExtendedEdge) g else g
        g'' = connectBlocks g'
    in graphToDot params g''

isExtendedEdge (ExtendedDependency, _, _) = True
isExtendedEdge _                          = False

connectBlocks g =
    let cs    = components g
        cIns  = map (findInComponent isIn g) cs
        cOuts = map (findInComponent isOut g) cs
        es    = zip cOuts (tail cIns)
        edges = [(p, c, (FakeDependency, False, [Nothing])) | (p, c) <- es]
    in insEdges edges g

findInComponent f g = fromJust . find (f . toIstr g)

params :: (Eq i, Show i, Ord r, Show r) => GraphvizParams n (BlockOperation i r)
          (DGEdgeLabel r) () (BlockOperation i r)
params = nonClusteredParams {
           globalAttributes = [GraphAttrs [RankSep [0.02]]]
         , fmtNode = dgNodeAttributes
         , fmtEdge = dgEdgeAttributes
}

optionalColor = Gray40
optionalColorLighter = Gray60
optionalColorLightest = Gray70

dgNodeAttributes (_, i) =
    let l  = "o" ++ show (oId i) ++ ": " ++ showInstrs (oOpr i)
        fc = X11Color (if isMandatory i then Black else optionalColor)
        c  = X11Color (if isMandatory i then Black else optionalColorLightest)
    in [toLabel l, FontName graphFontName, FontColor fc, shape BoxShape,
        Color [toWC c]]

dgEdgeAttributes (_, _, (DataDependency t, m, ls)) =
    anyEdgeAttributes m (showLatencies ls ++ "\n(" ++ showTemps t ++ ")") Nothing

dgEdgeAttributes (_, _, (ReadWriteDependency rwo a, m, ls)) =
    anyEdgeAttributes m
    (showLatencies ls ++ "\n(" ++ showAccess a ++ ": " ++ show rwo ++ ")")
    Nothing

dgEdgeAttributes (_, _, (ControlDependency, m, ls)) =
    anyEdgeAttributes m (showLatencies ls) (Just (Style [SItem Dashed []]))

dgEdgeAttributes (_, _, (ExtendedDependency, m, ls)) =
    anyEdgeAttributes m (showLatencies ls) extDepAttrs

dgEdgeAttributes (_, _, (FakeDependency, _, _)) = [Style [SItem Invisible []]]

anyEdgeAttributes m label style =
    let fc = X11Color (if m then Black else optionalColor)
        c  = X11Color (if m then Black else optionalColorLighter)
    in [toLabel label, FontName graphFontName, FontColor fc, Color [toWC c]]
           ++ maybeToList style

extDepAttrs = Just (Style [SItem Dotted []])

showLatencies [l] = showLatency l
showLatencies ls  = showAlternatives showLatency ls

showLatency (Just l) = show l
showLatency Nothing  = "-"

showTemps [t] = show t
showTemps ts  = renderStyle (st 20) (cs show ts)
