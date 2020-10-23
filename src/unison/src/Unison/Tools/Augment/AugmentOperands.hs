{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Augment.AugmentOperands (augmentOperands) where

import Data.List
import Data.Graph.Inductive
import Data.Maybe

import Common.Util

import Unison
import Unison.Target.API
import qualified Unison.Graphs.CG as CG
import Unison.Graphs.Util
import Unison.Graphs.ThirdParty

augmentOperands noCross oldModel f @ Function {fCode = code} target =
    let atf   = alternativeTemps target
        code' = map (augmentOperandsInBlock atf noCross oldModel) code
    in f {fCode = code'}

augmentOperandsInBlock atf noCross oldModel b @ Block {bCode = code} =
  let cg    = mkCopyGraph code
      code' = map (augmentOperandsInOpr atf noCross oldModel code cg) code
  in b {bCode = code'}

augmentOperandsInOpr atf noCross oldModel code cg o =
    let o' = mapToEachUse (augmentOperand atf noCross oldModel code cg o) o
        nf = if oldModel then id else maybeAddNullTemp o
    in mapToEachDef nf o'

augmentOperand atf noCross oldModel code cg o p @ MOperand {altTemps = [t]} =
  let cs   = componentsOf cg
      tId  = CG.toNodeId t
      cg'  = fromJust $ find (\g -> tId `elem` nodes g) cs
      cg'' = maybeRemoveDeeperThan o cg'
      c    = if noCross then sort $ reachable tId (grev cg'') else nodes cg''
      nt   = if nullable o then toSingleton mkNullTemp else []
      ats  = [(t, Nothing) | t <- nt] ++
             [(t, Just (definer t code)) | t <- map mkTemp c]
      ts   = if oldModel then [undoPreAssign t] else atf code o p ats
      p'   = p {altTemps = ts}
  in p'

augmentOperand _ _ _ _ _ _ u = u

maybeAddNullTemp o p @ MOperand {altTemps = ts}
  | nullable o = p {altTemps = mkNullTemp : ts}
maybeAddNullTemp _ p = p

maybeRemoveDeeperThan o cg' =
    case find (isOperationEdge (oId o)) (labEdges cg') of
      (Just (p, _, _)) -> removeDeeperThan p cg'
      Nothing          -> cg'

isOperationEdge i (_, _, i') = i == i'

nullable o = not $ isMandatory o

mapToEachUse f = mapToOperands (map f) id
mapToEachDef f = mapToOperands id (map f)

mkCopyGraph code =
  let nodes = nub $ [(toNodeId t, ()) | t <- allSingleTemps code]
      edges = [(toNodeId $ copySource o, toNodeId $ copyDestination o, oId o)
              | o <- code, isCopy o]
  in mkGraph nodes edges :: Gr () OperationId

toNodeId (Temporary t _) = fromIntegral t
toNodeId (MOperand {altTemps = [t]}) = toNodeId t

allSingleTemps code =
  concat [[t | MOperand {altTemps = [t]} <- oAllOps o] | o <- code]
