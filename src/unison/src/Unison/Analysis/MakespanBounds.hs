{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Algorithms to compute makespan bounds.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.MakespanBounds
    (computeMaxC, maxCost, maxInt, scaleFactor, scaleDown) where

import Data.Maybe
import Data.Int
import Data.List
import Data.Graph.Inductive

import Unison
import Unison.Target.Query
import qualified Unison.Graphs.DG as DG

computeMaxC targetFs Block {bCode = code} =
    sum $ map (maxInstructionLatency targetFs) code

maxInstructionLatency targetFs o
    | isVirtual o = 1
    | isBundle o =
        maximum $ map (maxInstructionLatency targetFs) (bundleOs o)
    | otherwise =
        maximum $ mapMaybe (worst targetFs o) (oInstructions o)

worst _ _ (General NullInstruction) = Nothing
worst (rm, oif, dgs) o ti @ (TargetInstruction i) =
    let maxDur     = maxOrZero $ map (occupation . usage) (iUsages rm ti)
        useLats    = operandLats $ fst $ oif i
        defLats    = operandLats $ snd $ oif i
        maxOperLat = maxOrZero useLats + maxOrZero defLats
        deps       = DG.dependencies $ fromJust $ find (containsOperation o) dgs
        ii         = fromJust $ elemIndex ti (oInstructions o)
        outgoing   = [dist !! ii | (p, _, dist) <- deps, oId o == p]
        incoming   = [dist | (_, s, dist) <- deps, oId o == s]
        maxDist    = maxOrZero $ catMaybes $ concat incoming ++ outgoing
        maxMinLive = if any isModelOperand (oDefs o) then minLiveOfDefs o else 0
    in Just $ maximum [maxDur, maxOperLat, maxDist, maxMinLive]

operandLats oifs = [l | TemporaryInfo {oiLatency = l} <- oifs]

containsOperation o dg = oId o `elem` map (oId . snd) (labNodes dg)

maxOrZero = maybeMax 0

maxCost targetFs code =
  let maxc    = map (computeMaxC targetFs) code
      rawfreq = map blockFreq code
  in sum $ map (\(f, m) -> f * m) (zip rawfreq maxc)

maxInt = toInteger (maxBound - 1 :: Int32)

scaleFactor targetFs code =
  let max    = maxCost targetFs code
      factor = toRational maxInt / toRational max
  in if max > maxInt then factor else 1.0

scaleDown factor = atLeast 1 . floor . (*) factor . toRational

atLeast k n
  | n < k     = k
  | otherwise = n
