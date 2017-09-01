{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Hoopl-based reaching constants on SSA pass.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE GADTs #-}
module Unison.Graphs.Hoopl.ReachingConstantsSSA (reachingConstants) where

import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Compiler.Hoopl hiding (joinFacts)

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Graphs.Hoopl

-- | Reaching constants analysis (based on Briggs et al.'s data-flow equations)

data ConstLattElem i r =
    -- | No info is known about the temporary
    ConstTop |
    -- | The temporary can be rematerialized by the given operation
    ConstOpr {
      constOpr  :: BlockOperation i r,
      constDefs :: S.Set OperationId
    } |
    -- | The temporary cannot be rematerialized
    ConstBottom
    deriving (Eq, Ord, Show)

isConstOpr ConstOpr {} = True
isConstOpr _ = False

type ReachingConstants i r = M.Map (Operand r) (ConstLattElem i r)

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
meet (ConstOpr o ds) (ConstOpr o' ds')
  | equivalent o o' = ConstOpr o (S.union ds ds')
  | otherwise = ConstBottom
meet x y = error $ "unmatched: meet " ++ show x ++ " " ++ show y

equivalent o o' = oOpr o == oOpr o'

canonicalize const @ ConstOpr {constOpr = o} =
    let o'  = renameOperationId $ renameTemps o
    in const {constOpr = o'}

renameTemps o =
    let tmap = M.fromList (zip (map tId $ tUniqueOps [o]) [0..])
        o'   = mapToModelOperand (applyTempIdMap tmap) o
    in o'

renameOperationId o = o {oId = 0}

-- | Transfer function of an operation
constantsTransfer :: Ord i => Show i => Ord r => Show r =>
                     FwdTransfer (HOperation i r) (ReachingConstants i r)
constantsTransfer = mkFTransfer3 (const id) middleConstants outConstants

middleConstants :: Ord i => Show i => Ord r => Show r => HOperation i r O O ->
                   ReachingConstants i r -> ReachingConstants i r
middleConstants (HMiddle o) consts =
    let ds      = concatMap extractTemps (oDefs o)
        e       = if isRematerializable o && length ds == 1
                  then mkLatticeElement consts o
                  else ConstBottom
        new     = M.fromList $ zip ds (repeat e)
        consts' = joinFacts new consts
    in consts'

mkLatticeElement consts o =
    case concatMap extractTemps (oUses o) of
      [] -> canonicalize (ConstOpr o (S.fromList [oId o]))
      us ->
        let uconsts = [consts M.! t | t <- us]
        in (case (all isConstOpr uconsts, nub (map constOpr uconsts)) of
             (True, [ro]) | isVirtualCopy o || isPhi o ->
                            ConstOpr ro (S.unions $ map constDefs uconsts)
             _  -> ConstBottom)

-- TODO: check with target, extract info from TableGen's 'isReMaterializable'.
isRematerializable o | isDefine o = False
isRematerializable _ = True

outConstants :: Ord i => Show i => Ord r => Show r => HOperation i r O C ->
                ReachingConstants i r -> FactBase (ReachingConstants i r)
outConstants (HOut _ s) consts =
    mkFactBase (constantsLattice []) (zip s (repeat consts))

constantsPass ts = FwdPass {
             fp_lattice = constantsLattice ts,
             fp_transfer = constantsTransfer,
             fp_rewrite = noFwdRewrite
           }

-- | Partial map from temporaries that can be computed from never-killed values
-- (i.e. "rematerializable" temporaries) to the operations that can compute
-- their values.
reachingConstants cfg ts =
    let b2rcs = reachingConsts cfg ts
        lastb = maximum $ setElems $ labelsDefined cfg
        rts   = fromJust $ lookupFact lastb b2rcs
        rts'  = M.filter isConstOpr rts
        rts'' = M.map (\c -> (constOpr c, constDefs c)) rts'
    in rts''

reachingConsts cfg ts = runSimpleUniqueMonad $ do
  (_, rts, NothingO) <- reachingConsts1 cfg ts
  return rts

reachingConsts1 :: Ord i => Show i => Ord r => Show r =>
                  Graph (HOperation i r) C C -> [Operand r] ->
                  SimpleUniqueMonad
                  (Graph (HOperation i r) C C, FactBase (ReachingConstants i r),
                   MaybeO C (ReachingConstants i r))
reachingConsts1 cfg ts = analyzeAndRewriteFwd (constantsPass ts)
               (JustC [labelToHLabel 0]) cfg mapEmpty
