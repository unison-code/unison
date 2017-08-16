{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Hoopl-based reaching definitions pass.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE GADTs #-}
module Unison.Graphs.Hoopl.ReachingDefinitions
    (ReachingDefinition(..),
     reachingDefinitions) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Compiler.Hoopl hiding (joinFacts)

import Unison.Base
import Unison.Util
import Unison.Graphs.Hoopl

-- | Reaching definitions analysis

type OperandPredicate r = (Operand r -> Bool)

data ReachingDefinition r =
    ReachingDefinition {
      rdVariable :: Operand r,
      rdDefiner  :: OperationId
    } deriving (Eq, Ord)

instance Show r => Show (ReachingDefinition r) where
  show (ReachingDefinition r d) = show r ++ "(o" ++ show d ++ ")"

type ReachingDefs r = S.Set (ReachingDefinition r)

reachDefLattice :: Ord r => DataflowLattice (ReachingDefs r)
reachDefLattice = DataflowLattice {
                  fact_name = "reaching definitions analysis",
                  fact_bot  = S.empty,
                  fact_join = const join
                }

join :: Ord r =>
        (OldFact (ReachingDefs r)) ->
        (NewFact (ReachingDefs r)) ->
        (ChangeFlag, (ReachingDefs r))
join (OldFact old) (NewFact new) =
    let joined = new `S.union` old
        ch     = changeIf (S.size joined > S.size old)
    in (ch, joined)

-- | Transfer function of an operation
reachDefTransfer :: Ord i => Show i => Ord r =>
                    OperandPredicate r ->
                    FwdTransfer (HOperation i r) (ReachingDefs r)
reachDefTransfer p = mkFTransfer3 inDefs (middleDefs p) outDefs

inDefs :: Ord i => Show i => HOperation i r C O -> ReachingDefs r -> ReachingDefs r
inDefs _ rds = rds

middleDefs :: Ord i => Show i => Ord r =>
              OperandPredicate r ->
              HOperation i r O O -> ReachingDefs r -> ReachingDefs r
middleDefs p (HMiddle o) rds =
    S.union (gen p o) (S.difference rds (kill p rds o))

outDefs :: Ord i => Show i => Ord r =>
           HOperation i r O C -> ReachingDefs r -> FactBase (ReachingDefs r)
outDefs (HOut _ s) rds = mkFactBase reachDefLattice (zip s (repeat rds))

gen p o = S.fromList [ReachingDefinition r (oId o) | r <- filter p (oDefs o)]

kill p rds o =
    let ds = map rdVariable $ S.toList $ gen p o
    in S.filter (\rd -> rdVariable rd `elem` ds) rds

reachingDefinitions p cfg =
    let b2rcs  = reachingDefs p cfg
        ls     = setElems (labelsDefined cfg)
        b2rcs' = M.fromList [(hLabelToLabel l, fact b2rcs l) | l <- ls]
    in b2rcs'

fact f l = fromMaybe S.empty $ lookupFact l f

reachingDefs p cfg = runSimpleUniqueMonad $ do
  (_, rts, NothingO) <- reachDefs p cfg
  return rts

reachDefs :: Ord i => Show i => Ord r =>
             OperandPredicate r -> Graph (HOperation i r) C C ->
             SimpleUniqueMonad
             (Graph (HOperation i r) C C,
              FactBase (ReachingDefs r), MaybeO C (ReachingDefs r))
reachDefs p cfg =
    let reachDefPass = FwdPass {
               fp_lattice  = reachDefLattice,
               fp_transfer = reachDefTransfer p,
               fp_rewrite  = noFwdRewrite
             }
    in  analyzeAndRewriteFwd reachDefPass
            (JustC [labelToHLabel 0]) cfg mapEmpty
