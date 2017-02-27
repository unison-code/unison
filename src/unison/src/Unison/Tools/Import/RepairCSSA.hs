{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Import.RepairCSSA (repairCSSA) where

import Data.Ord
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

import Common.Util
import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import Unison.Graphs.Hoopl
import Unison.Graphs.Hoopl.Liveness
import qualified Unison.Graphs.Partition as P
import Unison.Transformations.PropagatePhiCongruences

-- | This algorithm for CSSA reparation is based on Method III from (Sreedhar,
-- 1999). The problem of not always being able to place use copies at the very
-- end of a block is addressed in (Boissinot, 2009) but not currently handled
-- here.

repairCSSA :: (Ord i, Show i, Ord r, Show r) =>
              Function i r -> TargetWithOptions i r rc s -> Function i r
repairCSSA f @ Function {fCode = code} target =
    let bcfg          = BCFG.fromFunction (branchInfo target) f
        phiInsts      = sort $ filter isPhi (flatten code)
        phiCongrClass = mkPhiCongruenceMap phiInsts :: Partition TemporaryId
        live          = liveTemporaries f target
        (code', _, _) = foldl (repairCSSAForPhi bcfg)
                              (code, live, phiCongrClass) phiInsts
    in f {fCode = code'}

-- | Updates code, live and the phi-congruence classes with the copies
-- | necessaries to break interferences related to phiInst.
repairCSSAForPhi bcfg (code, live, phiCongrClass) phiInst =
    -- phiInst in the form of x0 = f(x1:l1, x2:l2, ..., xn:ln)
    let phiId               = oId phiInst
        candidateResSet     = S.empty
        xs                  = phiTemps phiInst code
        unresNeighborMap    = M.fromList [(x, S.empty) | (x, _) <- xs]
        -- pairs of resources xi:li and xj:lj in phiInst, where xi != xj, such
        -- that there exists yi in phiCongrClass[xi], yj in phiCongrClass[xj],
        -- and yi and yj interfere with each other
        phiPairs            = [((xi, li), (xj, lj))
                                   | (xi, li) <- xs, (xj, lj) <- xs, xi < xj,
                                     interfere live phiCongrClass (xi, xj)]
        (candidateResSet',
         unresNeighborMap') = foldl (copiesForPair live phiCongrClass)
                                    (candidateResSet, unresNeighborMap) phiPairs
        candidateResSet''   = resolveCopies unresNeighborMap' candidateResSet'
        (code', live',
         phiCongrClass')    = foldl (insertCopy bcfg phiId xs)
                              (code, live, phiCongrClass)
                              (S.toList candidateResSet'')
        -- Merge phiCongruenceClass's for all resources in phiInst
        xs'                 = tOps [findOpr phiId code']
        phiCongrClass''     = foldl P.connectElements phiCongrClass'
                              [(tId xi, tId xj) | xi <- xs', xj <- xs']
    in (code', live', phiCongrClass'')

-- | Whether there exists yi in phiCongrClass[xi], yj in phiCongrClass[xj] and
-- yi and yj interfere with each other.
interfere (liveIn, liveOut) phiCongrClass (xi, xj) =
    let cxi  = classTemps phiCongrClass xi
        cxj  = classTemps phiCongrClass xj
        iIn  = any (tempIntersection (cxi, cxj)) (M.toList liveIn)
        iOut = any (tempIntersection (cxi, cxj)) (M.toList liveOut)
    in iIn || iOut

tempIntersection (cxi, cxj) (_, ts) =
    let ts'  = S.difference ts cxi
        ts'' = S.difference ts' cxj
    in S.size ts'' < S.size ts' && S.size ts' < S.size ts

-- | Updates candidateResSet and unresNeighborMap with the copies needed to
-- break the interference between xi and xj using the four cases described in
-- Section 4.3 of (Sreedhar, 1999).
copiesForPair (_, liveOut) phiCongrClass (candidateResSet, unresNeighborMap)
                  ((xi, li), (xj, lj)) =
    let intXi = S.intersection (classTemps phiCongrClass xi) (liveOut M.! lj)
        intXj = S.intersection (classTemps phiCongrClass xj) (liveOut M.! li)
    in phiCopies (candidateResSet, unresNeighborMap) (xi, intXi) (xj, intXj)

phiCopies (candidateResSet, unresNeighborMap) (xi, intXi) (xj, intXj)
    -- Case 1. intXi is not empty, and intXj is empty.  A new copy, xi'=xi, is
    -- needed in li to ensure that xi and xj are put in different phi congruence
    -- classes. So xi is added to candidateResSet.
    | not (S.null intXi) && S.null intXj =
        (S.insert xi candidateResSet, unresNeighborMap)

    -- Case 2. intXi is empty, and intXj is not empty. A new copy, xj'=xj, is
    -- needed in lj to ensure that xi and xj are put in different phi congruence
    -- classes. So xj is added to candidateResSet.
    | S.null intXi && not (S.null intXj) =
        (S.insert xj candidateResSet, unresNeighborMap)

    -- Case 3. intXi is not empty, and intXj is not empty. Two new copies,
    -- xi'=xi in li and xj'=xj in lj, are needed to ensure that xi and xj are
    -- put in different phi congruence classes. So xi and xj are added to
    -- candidateResSet.
    | not (S.null intXi) && not (S.null intXj) =
        (inserts [xi, xj] candidateResSet, unresNeighborMap)

    -- Case 4. intXi is empty, and intXj is empty. Either a copy, xi'=xi in
    -- li, or a copy, xj'=xj in lj, is sufficient to eliminate the interference
    -- between xi and xj. However, the final decision of which copy to insert is
    -- deferred until all pairs of interfering resources in the phi instruction
    -- are processed.
    | S.null intXi && S.null intXj =
        let unresNeighborMap'  = M.adjust (S.insert xi) xj unresNeighborMap
            unresNeighborMap'' = M.adjust (S.insert xj) xi unresNeighborMap'
        in (candidateResSet, unresNeighborMap'')

-- | Compute which of the temporaries that have been deferred should be added to
-- | the candidate resource set
resolveCopies unresNeighborMap candidateResSet =

    let -- Resources in the candidate resource set are considered as resolved.
        unresNeighborMap'      = M.map (remove candidateResSet) unresNeighborMap
        -- Pick resources from the map in a decreasing size of unresolved resource
        -- set.
        unresNeighbors         = reverse $ sortBy (comparing (S.size . snd))
                                 (M.toList unresNeighborMap')
        -- For each resource x that is picked up from the map, add x to
        -- candidateResSet if x contains at least one unresolved neighbor. Also
        -- mark x to be resolved (remove it from all the sets in
        -- candidateResSet?).
        (candidateResSet'', _) = foldl resolve
                                 (candidateResSet, unresNeighborMap')
                                 (fst $ unzip unresNeighbors)
        -- TODO: Finally, when all the maps are processed, it is possible a
        -- resource x that was marked as resolved may now contain all its
        -- neighbors to be marked as resolved. If this is the case we remove x
        -- from candidateResSet.
    in candidateResSet''

resolve (candidateResSet, unresNeighborMap) x
    | S.null (unresNeighborMap M.! x) = (candidateResSet, unresNeighborMap)
    | otherwise =
        let candidateResSet'  = S.insert x candidateResSet
            unresNeighborMap' = M.map (S.delete x) unresNeighborMap
        in (candidateResSet', unresNeighborMap')

insertCopy bcfg phiId xs (code, live, phiCongrClass) xi =
    let phiInst = findOpr phiId code
    in if phiTarget phiInst == xi
       then insertTargetCopy (oId phiInst) xi (code, live, phiCongrClass)
       else -- For every lk associated with xi in the source list of phiInst
       foldl (insertSourceCopy bcfg (oId phiInst) xi)
                 (code, live, phiCongrClass) ls
           where ls = [lk | (x, lk) <- xs, x == xi]

-- | Inserts a copy for x0 (the phi target)
insertTargetCopy phiId x0 (code, (liveIn, liveOut), phiCongrClass) =
    let xs             = phiTemps (findOpr phiId code) code
        --Insert a copy inst: x0 = xnew_0 at the beginning of L0
        xnew_0         = newTemp code
        l0             = fromSingleton [l | (x, l) <- xs, x == x0]
        code'          = insertCopyInBlock after thePhi code l0 (xnew_0, x0)
        -- Replace x0 with xnew_0 as the target in phiInst
        code''         = mapToOperationInBlocks
                         (replacePhiTemp phiId xs (x0, l0) (xnew_0, l0))
                         code'
        -- Add xnew_0 in phiCongruenceClass[xnew_0]
        phiCongrClass' = P.addElement phiCongrClass (tId xnew_0)
        -- Update liveness information
        liveIn'        = M.adjust (S.delete x0) l0 liveIn
        liveIn''       = M.adjust (S.insert xnew_0) l0 liveIn'
    in (code'', (liveIn'', liveOut), phiCongrClass')

-- | Inserts a copy for xi (a source resource of the phi instruction)
insertSourceCopy bcfg phiId xi (code, (liveIn, liveOut), phiCongrClass) lk =
    let xs             = phiTemps (findOpr phiId code) code
        -- Insert a copy inst: xnew_i = xi at the end of lk
        xnew_i         = newTemp code
        code'          = insertCopyInBlock before theOut code lk (xi, xnew_i)
        -- Replace xi with xnew_i in phiInst
        code''         = mapToOperationInBlocks
                         (replacePhiTemp phiId xs (xi, lk) (xnew_i, lk))
                         code'
        -- Add xnew_i in phiCongruenceClass[xnew_i]
        phiCongrClass' = P.addElement phiCongrClass (tId xnew_i)
        -- Update liveness information
        -- liveOut[lk] += xnew_i
        liveOut'       = M.adjust (S.insert xnew_i) lk liveOut
         -- if (for lj an immediate successor of lk, xi not in liveIn[lj] and
         --     not used in a phi instruction associated with lk in lj)
         --       liveOut[lk] -= xi
        bjs            = [code'' !! lj | lj <- BCFG.immediateSuccessors bcfg lk]
        liveOut''      = if none (usedInBlock liveIn xi lk) bjs then
                            M.adjust (S.delete xi) lk liveOut' else liveOut'
    in (code'', (liveIn, liveOut''), phiCongrClass')

-- | Inserts a copy (t, t') in the place given by (w, f) in block bId
insertCopyInBlock w f code bId (t, t') =
    let id               = newId code
        [pre, [b], post] = split (condense $ whenElt ((==) bId . bLab)) code
        c                = mkVirtualCopy id t t'
        b'               = insertOperationInBlock w (f (t, t')) c b
        code'            = pre ++ [b'] ++ post
    in code'

thePhi = isDefiner . snd
theOut = const isOut

-- | Whether xi is in liveIn[lj] or used in a phi instruction associated with lk
usedInBlock liveIn xi lk Block {bLab = lj, bCode = code} =
    let phiInsts  = filter isPhi code
        usedInPhi = any (phiUserOf (xi, lk)) phiInsts
    in S.member xi (liveIn M.! lj) || usedInPhi

phiUserOf (x, l) phiInst = (x, l) `elem` phiUses phiInst

-- | Replaces the given (temp, label) in the phiId
replacePhiTemp phiId xs (x, l) (x', l') bi
  | isId phiId bi =
      let (Virtual i) = oOpr bi
          tMap        = M.fromList [((x, l), (x', l'))]
          (x0':xs')   = map (applyMap tMap) xs
          i'          = i {oPhiUs = flattenPhiUses xs', oPhiD = fst x0'}
      in bi {oOpr    = Virtual i'}
  | otherwise       = bi

flattenPhiUses xs = concat [[xi, BlockRef li] | (xi, li) <- xs]

-- | Constructs the initial phi-congruence class map
mkPhiCongruenceMap phiInsts =
    let xs = nub $ tOps phiInsts
    in P.fromNodes (map tId xs)

-- | Gives liveness information for code with phi instructions following the
-- liveness definition given in Section 3 of (Sreedhar, 1999).
liveTemporaries f target =
  let f'      = propagatePhiCongruences f target
      bif      = branchInfo target
      cfg     = toHGraph bif f'
      b2ts    = liveTemps cfg
      b2ts'   = foldl promoteDelimiters b2ts (fCode f')
      liveIn  = M.fromList [(b, lIn) | (b, (lIn, _)) <- M.toList b2ts']
      liveOut = M.fromList [(b, lOut) | (b, (_, lOut)) <- M.toList b2ts']
  in (liveIn, liveOut)

promoteDelimiters b2ts b @ Block {bLab = l} =
  let (tIn, tOut) = b2ts M.! l
      tIn'        = S.union tIn (toSimpleTemps $ oDefs $ blockIn b)
      tOut'       = S.union tOut (toSimpleTemps $ oUses $ blockOut b)
  in M.insert l (tIn', tOut') b2ts

toSimpleTemps :: Ord r => [Operand r] -> S.Set (Operand r)
toSimpleTemps = S.fromList . map (mkTemp . tId)

phiTarget phi = fromSingleton $ tDefs [phi]

definerBlock code t = bLab $ tempBlock code t

inserts :: Ord r => [Operand r] -> S.Set (Operand r) -> S.Set (Operand r)
inserts = S.union . S.fromList

remove :: Ord r => S.Set (Operand r) -> S.Set (Operand r) -> S.Set (Operand r)
remove = flip S.difference

newTemp = mkTemp . newTempIndex . flatten

phiTemps phiInst code =
    let x0 = phiTarget phiInst
        l0 = definerBlock code x0
    in (x0, l0) : phiUses phiInst

findOpr id code = fromJust $ find (isId id) (flatten code)

classTemps phiCongrClass =
  S.fromList . map mkTemp . fromJust . P.equivalent phiCongrClass
