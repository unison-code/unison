{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Export.RemoveNops (removeNops) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Arrow

import Unison
import Unison.Target.API
import Unison.Target.Query
import Unison.Target.RegisterArray
import Unison.Analysis.Stalls

-- TODO: remove nops only when the latencies are "soft". Right now we
-- assume that def latencies are "soft" and use latencies are "hard",
-- but this information is really processor-dependent and should be
-- part of the description.

data CriticalResource r =
    CRRegister (Operand r) |
    CRRWObject (RWObject r)
    deriving (Eq, Ord, Show)

removeNops f @ Function {fCode = code} target =
    let oif   = operandInfo target
        rwlf  = readWriteLatency target
        iNop  = nop target
        uf    = usages target
        cf    = capacityMap target
        ra    = mkRegisterArray target 0
        ovf   = regOverlap (regAtoms ra)
        nbf   = isNopBundle iNop
        code' = map (removeBlockNops uf cf ovf nbf oif rwlf) code
    in f {fCode = code'}

removeBlockNops uf cf ovf nbf oif rwlf b @ Block {bCode = code} =
  let icode  = zip [1..] code
      deps   = foldr (criticalDependencies ovf oif) [] icode
      deps'  = map completeDep deps
      deps'' = deps' ++ rwDeps rwlf icode
      (_,
       usages) = mapAccumL (cycleUsages uf cf) [] icode
      icode' = compress nbf uf cf (M.fromList usages) icode deps''
      code'  = snd $ unzip icode'
  in b {bCode = code'}

criticalDependencies ovf oif (cycle, bundle) deps =
  let os      = bundleOs bundle
      newDeps = concatMap (newDependencies oif cycle) os
      defs    = concatMap (definedCRs oif) os
      deps'   = map (updateDependency ovf (cycle, defs)) deps
  in deps' ++ newDeps

newDependencies oif index o =
  let uinfo = fst $ oif (targetInst (oInstructions o))
  in [(CRRegister r, Nothing, Just (index, l), Nothing) |
      (TemporaryInfo {oiLatency = l}, r @ Register {}) <- zip uinfo (oUses o),
      l > 0]

definedCRs oif o =
  let dinfo = snd $ oif (targetInst (oInstructions o))
  in [(CRRegister r, l)
     | (TemporaryInfo {oiLatency = l}, r @ Register {}) <- zip dinfo (oDefs o)]

updateDependency _ _ dep @ (_, Just _, Just _, Just _) = dep
updateDependency ovf (cycle, defs) (r, Nothing, Just (u, ul), Nothing) =
  case find (\(r', _) -> aliasesWith ovf r r') defs of
    Just (_, dl) ->
      let slack = u - cycle - dl - ul
      in (r, Just (cycle, dl), Just (u, ul), Just slack)
    Nothing -> (r, Nothing, Just (u, ul), Nothing)

completeDep dep @ (_, Just _, Just _, Just _) = dep
completeDep (r, Nothing, Just (u, ul), Nothing) =
  (r, Just (0, 1), Just (u, ul), Just (u - ul))

rwDeps rwlf icode =
    let icode'    = map (second cleanRedundantReads) icode
        writeDeps = concatMap rwWrites icode'
        readDeps  = concatMap rwReads icode'
        deps      = mergeDeps rwlf (writeDeps, Write) (readDeps, Read) ++
                    mergeDeps rwlf (writeDeps, Write) (writeDeps, Write)
    in deps

rwWrites (cycle, bundle) =
    concat [[(fromSingleton $ oInstructions o, rwo,  cycle)
                 | rwo <- oWriteObjects o] | o <- bundleOs bundle]

rwReads (cycle, bundle) =
    concat [[(fromSingleton $ oInstructions o, rwo, cycle)
                 | rwo <- oReadObjects o] | o <- bundleOs bundle]

mergeDeps rwlf (deps1, a1) (deps2, a2) =
    [(CRRWObject rwo, Just (dc, rwLat rwlf rwo (di, a1) (ui, a2)), Just (uc, 0),
      Just (uc - (dc + (rwLat rwlf rwo (di, a1) (ui, a2)))))
     | (di, rwo,  dc) <- deps1, (ui, rwo', uc) <- deps2, rwo == rwo', uc > dc]

rwLat rwlf rwo (di, a1) (ui, a2) =
    fromJust $ rwlf rwo
    ((di, NaturalType LinearType), a1)
    ((ui, NaturalType LinearType), a2)

compress _ _ _ _ [] _ = []
compress nbf uf cf usages ((cycle, bundle) : rest) deps
  | nbf bundle =
    let deps'    = map (removeNop cycle) deps
        -- usages coming from previous cycles
        prev     = case M.lookup (max 0 (cycle - 1)) usages of
                        (Just res) -> res
                        Nothing -> []
        -- usages if the nop is removed
        usages'  = M.fromList $ snd $
                   mapAccumL (cycleUsages uf cf) prev rest
        -- combined usages if the nop is removed
        usages'' = map (combineAdd . map toMapTuple' . snd) $ M.toList usages'
        exceeds  = any (any (exceedsCapacity cf)) usages''
    in if any isNegativeSlack deps' || exceeds then
         (cycle, bundle) : compress nbf uf cf usages rest deps
       else compress nbf uf cf usages' rest deps'
  | otherwise = (cycle, bundle) : compress nbf uf cf usages rest deps

isNopBundle oNop (Bundle {bundleOs = [SingleOperation {oOpr = opr}]}) =
            oNop == opr
isNopBundle _ _ = False

removeNop cycle dep @ (r, Just (d, dl), Just (u, ul), Just s)
  | cycle > d && cycle < u = (r, Just (d, dl), Just (u, ul), Just (s - 1))
  | otherwise = dep

isNegativeSlack (_, _, _, Just s) = s < 0

aliasesWith ovf (CRRegister r) (CRRegister r') = ovf r r'
aliasesWith _ (CRRWObject rwo) (CRRWObject rwo') = rwo == rwo'
aliasesWith _ _ _ = False

cycleUsages uf cf acc (cycle, o)
  | not (isBundle o) = cycleUsages uf cf acc (cycle, mkBundle [o])
cycleUsages uf cf blocked (cycle, Bundle {bundleOs = bos}) =
  let blocked'  = filter isActive' $ map stepCycle' blocked
      newUsages = concatMap (resUsages' uf cf) bos
      blocked'' = blocked' ++ newUsages
  in (blocked'', (cycle, blocked''))

resUsages' uf cf o = map toSimple $ filter isNullOffset $ resUsages uf cf o
isNullOffset BlockingResourceState {brsOffset = off} = off == 0
stepCycle' = toSimple . stepCycle . fromSimple
isActive' = isActive . fromSimple
toMapTuple' = toMapTuple . fromSimple

-- TODO: use BlockingResourceState rather than (BlockingResource, Integer)
toSimple BlockingResourceState {brsResource = r, brsOccupation = occ} = (r, occ)
fromSimple (r, occ) = BlockingResourceState r occ 0
