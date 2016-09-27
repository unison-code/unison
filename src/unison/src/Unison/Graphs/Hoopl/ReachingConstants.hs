{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Hoopl-based reaching constants pass.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE GADTs #-}
module Unison.Graphs.Hoopl.ReachingConstants (reachingConstants) where

import Data.Maybe
import Data.List
import qualified Data.Map as M
import Compiler.Hoopl hiding (joinFacts)

import Common.Util
import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Graphs.Hoopl

-- | Reaching constants analysis

data ConstLattElem i r =
    -- | No info is known about the temporary
    ConstTop |
    -- | The temporary can be rematerialized by the given operations
    -- FIXME: extend to arbitrary expressions
    --        (by now, we only allow chains, which is a slight improvement to
    --         Briggs et al.'s paper)
    ConstOprs {
      constOprs :: [BlockOperation i r]
    } |
    -- | The temporary cannot be rematerialized
    ConstBottom
    deriving (Eq, Ord, Show)

isConstOprs ConstOprs {} = True
isConstOprs _ = False

type ReachingConstants i r = M.Map (Operand r) (ConstLattElem i r)

type TempConstsMap i r = M.Map (Operand r) [Operand r]

constantsLattice :: Ord i => Show i => Ord r => Show r => [Operand r] ->
                    DataflowLattice (ReachingConstants i r)
constantsLattice ts = DataflowLattice {
                fact_name = "reaching constants analysis",
                fact_bot  = M.fromList [(t, ConstTop) | t <- ts],
                fact_join = const join
              }

join :: Ord i => Show i => Ord r => Show r =>
        (OldFact (ReachingConstants i r)) ->
        (NewFact (ReachingConstants i r)) ->
        (ChangeFlag, ReachingConstants i r)
join (OldFact old) (NewFact new) =
    let joined = joinFacts new old
        ch     = changeIf ((M.toList joined) /= (M.toList old))
    in (ch, joined)

joinFacts xs ys = M.unionWith sortMeet xs ys

sortMeet x y =
    let [x1, y1] = sort [x, y]
    in meet x1 y1

meet ConstTop    ConstTop    = ConstTop
meet ConstBottom ConstBottom = ConstBottom
meet ConstTop    x           = x
meet _           ConstBottom = ConstBottom
meet (ConstOprs os) (ConstOprs os')
  | equivalent os os' = (ConstOprs os)
  | otherwise = ConstBottom
meet x y = error $ "unmatched: meet " ++ show x ++ " " ++ show y

equivalent os os' =
    let oprs   = map oOpr os
        oprs'  = map oOpr os'
    in oprs == oprs'

canonicalize :: Show i => Eq i => Ord i => Ord r => ConstLattElem i r ->
                ConstLattElem i r
canonicalize const @ ConstOprs {constOprs = os} =
    let os'   = sortBy canonicalCompare os
        os''  = renameTemps os'
        os''' = renameOperations os''
    in const {constOprs = os'''}

-- | Compares two operations by:
--   1. dependencies (definer first)
--   2. instructions (alphabetically)
--   3. non-temporary operands
canonicalCompare o o' =
  let opr  = oOpr o
      opr' = oOpr o'
  in case depCompare opr opr' of
        EQ -> case instrCompare opr opr' of
              EQ -> opersCompare opr opr'
              c' -> c'
        c  -> c

depCompare opr opr'
    | dependent opr opr' = LT
    | dependent opr' opr = GT
    | otherwise = EQ

dependent d u =
    let temps = filter isTemporary
        uses  = temps $ oOprUses u
        defs  = temps $ oOprDefs d
    in any (\t -> t `elem` defs) uses

instrCompare opr opr' = compare (oOprInstructions opr) (oOprInstructions opr')

opersCompare opr opr' =
    let noTemps = filter (not . isTemporary) . oOprUses
    in compare (noTemps opr) (noTemps opr')

renameTemps os =
    let tmap  = M.fromList (zip (nub $ temporaries os) [0..])
        os'   = map (mapToModelOperand(applyTempIdMap tmap)) os
    in os'

renameOperations os =
    let omap = M.fromList (zip (map oId os) [0..])
        os'  = map (mapToOperationId (applyMap omap)) os
    in os'

-- | Transfer function of an operation
constantsTransfer :: Ord i => Show i => Ord r => Show r =>
  TempConstsMap i r -> FwdTransfer (HOperation i r) (ReachingConstants i r)
constantsTransfer t2cs =
    mkFTransfer3 (inConstants t2cs) middleConstants outConstants

inConstants :: Ord i => Show i => Ord r => Show r => TempConstsMap i r ->
               HOperation i r C O -> ReachingConstants i r ->
               ReachingConstants i r
inConstants t2cs (HIn _ o) consts =
    let ts      = concatMap extractTemps (oDefs o)
        consts' = foldl (inConstantsForTemp t2cs) consts ts
    in consts'

inConstantsForTemp t2cs consts t =
    case t2cs M.! t of
      [] ->  let new     = M.fromList [(t, ConstBottom)]
                 consts' = joinFacts new consts
             in consts'
      cs ->
          let consts' = foldl (inConstantsForCongrTempPair t) consts cs
          in consts'

inConstantsForCongrTempPair t consts t' =
    let new = M.fromList [(t, consts M.! t')]
        consts' = joinFacts new consts
    in consts'

middleConstants :: Ord i => Show i => Ord r => Show r => HOperation i r O O ->
                   ReachingConstants i r -> ReachingConstants i r
middleConstants (HMiddle o) consts =
    let ds      = concatMap extractTemps (oDefs o)
        -- FIXME: extend to multiple operations and multidef operations
        e       = if isRematerializable o && length ds == 1
                  then mkLatticeElement consts o
                  else ConstBottom
        new     = M.fromList $ zip ds (repeat e)
        consts' = joinFacts new consts
    in consts'

mkLatticeElement consts o =
    case concatMap extractTemps (oUses o) of
      [] -> canonicalize (ConstOprs [o])
      us -> case nub [consts M.! t | t <- us] of
              [e] -> case e of
                       ConstOprs {constOprs = os} ->
                           -- FIXME: fix resulting expression chains
                           canonicalize (ConstOprs ([o] ++ os))
                       _ -> e
              _  -> ConstBottom -- FIXME: extend to arbitrary expressions

-- TODO: check with target, extract info from TableGen's 'isReMaterializable'.
isRematerializable o | isDefine o = False
isRematerializable _ = True

outConstants :: Ord i => Show i => Ord r => Show r => HOperation i r O C ->
                ReachingConstants i r -> FactBase (ReachingConstants i r)
outConstants (HOut _ s) consts =
    mkFactBase (constantsLattice []) (zip s (repeat consts))

constantsPass ts t2cs = FwdPass {
             fp_lattice = constantsLattice ts,
             fp_transfer = constantsTransfer t2cs,
             fp_rewrite = noFwdRewrite
           }

-- | Partial map from temporaries that can be computed from never-killed values
-- (i.e. "rematerializable" temporaries) to the operations that can compute
-- their values. The input function embedded in the argument cfg is in the
-- primitive form (i.e. no optional operations and only temporaries as operands)
-- and does not contain pre-assignments.
reachingConstants cfg ts t2cs =
    let b2rcs = reachingConsts cfg ts t2cs
        lastb = maximum $ setElems $ labelsDefined cfg
        rts   = fromJust $ lookupFact lastb b2rcs
        rts'  = M.filter isConstOprs rts
        rts'' = M.map constOprs rts'
    in rts''

reachingConsts cfg ts t2cs = runSimpleUniqueMonad $ do
  (_, rts, NothingO) <- reachingConsts1 cfg ts t2cs
  return rts

reachingConsts1 :: Ord i => Show i => Ord r => Show r =>
                  Graph (HOperation i r) C C -> [Operand r] ->
                  TempConstsMap i r ->
                  SimpleUniqueMonad
                  (Graph (HOperation i r) C C, FactBase (ReachingConstants i r),
                   MaybeO C (ReachingConstants i r))
reachingConsts1 cfg ts t2cs = analyzeAndRewriteFwd (constantsPass ts t2cs)
               (JustC [labelToHLabel 0]) cfg mapEmpty
