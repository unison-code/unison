{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Algorithms to compute register classes, and pre-assignments of a temporary
transitively through congruences.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.TransitiveOperations
    (ReachabilityType (..),
     transitiveRegClasses,
     transitivePreAssignments) where

import Data.List
import Data.Maybe
import Data.Graph.Inductive

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Target.Query
import Unison.Target.API
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.BCFG as BCFG

data ReachabilityType = Reaching | Reachable deriving (Show)

transitiveRegClasses
    :: Eq i => Show i => Ord r => Show r => Eq rc =>
       OperandInfoFunction i rc -> BCFGraph i r -> Partition (Operand r) ->
       ReachabilityType -> Function i r -> Operand r -> [RegisterClass rc]
transitiveRegClasses oif bcfg sg rt f t =
  case findPartitionOf sg t of
   Nothing -> []
   Just ts ->
     let fcode = flatCode f
         tos   = concatMap
                 (\t -> [(t, definer t fcode)] ++ [(t, u) | u <- users t fcode])
                 ts
         tos1  = filter (isNatural . snd) tos
         b     = tBlockLab f t
         tos2  = [(t, o) | (t, o) <- tos1,
                  evalRechability bcfg (oBlockLab f o) rt b]
         rcs   = nub [regClassOf oif o t | (t, o) <- tos2]
     in rcs

findPartitionOf sg t = find (\p -> t `elem` p) $ SG.sameTempPartitions sg

tBlockLab Function {fCode = code} t =
    bLab $ fromJust $ find (\b -> t `elem` tOps (bCode b)) code

oBlockLab Function {fCode = code} o =
    bLab $ fromJust $ find (\b -> o `elem` bCode b) code

evalRechability :: BCFGraph i r -> BlockId -> ReachabilityType -> BlockId ->
                   Bool
evalRechability bcfg b1 Reaching b2 =
    (BCFG.toNode b2) `elem` reachable (BCFG.toNode b1) bcfg
evalRechability bcfg b1 Reachable b2 = evalRechability bcfg b2 Reaching b1

transitivePreAssignments
    :: Eq i => Show i => Ord r => Show r => BCFGraph i r ->
       Partition (Operand r) -> ReachabilityType -> Function i r -> Operand r ->
       [Operand r]
transitivePreAssignments bcfg sg rt f t =
  case findPartitionOf sg t of
   Nothing -> []
   Just ts ->
     let ros  = concat
                [[(r, o) | t @ Temporary {tReg = Just r }
                           <- oAllOps o, t `elem` ts] | o <- flatCode f]
         b    = tBlockLab f t
         ros1 = [(r, o) | (r, o) <- ros,
                 evalRechability bcfg (oBlockLab f o) rt b]
         ros2 = nub (map fst ros1)
     in ros2
