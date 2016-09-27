{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Hoopl-based liveness pass.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE GADTs #-}
module Unison.Graphs.Hoopl.Liveness (liveTemps) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Compiler.Hoopl

import Unison.Base
import Unison.Util hiding (successors)
import Unison.Predicates
import Unison.Graphs.Hoopl

-- | Liveness analysis

type LiveTemps r = S.Set (Operand r)

liveLattice :: Ord r => DataflowLattice (LiveTemps r)
liveLattice = DataflowLattice {
                fact_name = "liveness analysis",
                fact_bot  = S.empty,
                fact_join = add
              }

-- | Tells how to add new facts to old facts
add _ (OldFact old) (NewFact new) =
    let j  = new `S.union` old
        ch = changeIf (S.size j > S.size old)
    in (ch, j)

-- | Transfer function of an instruction
liveTransfer :: Ord r => BwdTransfer (HOperation i r) (LiveTemps r)
liveTransfer = mkBTransfer3 inLive middleLive outLive

inLive :: Ord r => HOperation i r C O -> LiveTemps r -> LiveTemps r
inLive (HIn _ i) live = S.difference live (def i)

middleLive :: Ord r => HOperation i r O O -> LiveTemps r -> LiveTemps r
middleLive (HMiddle i) live = S.union (use i) (S.difference live (def i))

outLive :: Ord r => HOperation i r O C -> FactBase (LiveTemps r) -> LiveTemps r
outLive (HOut i s) l2live = S.union (use i) (S.unions $ map (fact l2live) s)

use i
  | isPhi i   = S.empty
  | otherwise = tSet oUses i
def i
  | isPhi i   = S.empty
  | otherwise = tSet oDefs i

tSet f = S.fromList . map undoPreAssign . filter isTemporary . f

fact f l = fromMaybe S.empty $ lookupFact l f

livePass :: Ord r => BwdPass SimpleUniqueMonad (HOperation i r) (LiveTemps r)
livePass = BwdPass {
             bp_lattice = liveLattice,
             bp_transfer = liveTransfer,
             bp_rewrite = noBwdRewrite
           }

-- | Gives a map from blocks to their (live-in, live-out) temporaries
liveTemps cfg =
  let lIn    = liveIns cfg
      ls     = setElems (labelsDefined cfg)
      body   = toBody cfg
      liveTs = M.fromList (map (toLiveTuple body lIn) ls)
  in liveTs

toLiveTuple body liveIn l =
    let hBlock = fromJust $ mapLookup l body
        succs  = successors hBlock
        tuple  = (fact liveIn l, S.unions $ map (fact liveIn) succs)
    in (hLabelToLabel l, tuple)

-- | Gives a map from in-delimiter nodes to their live-in temporaries
liveIns cfg = runSimpleUniqueMonad $ do
  (_, liveIn, _) <- liveness cfg
  return liveIn

liveness :: Show i => Ord r => Graph (HOperation i r) C C -> SimpleUniqueMonad
            (Graph (HOperation i r) C C, FactBase (LiveTemps r),
             MaybeO C (LiveTemps r))
liveness cfg = analyzeAndRewriteBwd livePass
               (JustC [labelToHLabel 0]) cfg mapEmpty
