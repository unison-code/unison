{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Algorithms to compute makespan bounds.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.MakespanBounds
    (computeMaxC, maxCost, maxInt, scaleFactor, scaleDown, minLiveOfDef) where

import Data.Maybe
import Data.Int
import Data.List

import Unison
import Unison.Target.Query

computeMaxC (rm, oif) (Block {bCode = code}, deps) =
  sum $ map (maxInstructionLatency (rm, oif, deps)) code

maxInstructionLatency targetFs o
    | isVirtual o = 1
    | isBundle o =
        maximum $ map (maxInstructionLatency targetFs) (bundleOs o)
    | otherwise =
        maximum $ mapMaybe (worst targetFs o) (oInstructions o)

worst _ _ (General NullInstruction) = Nothing
worst (rm, oif, deps) o ti @ (TargetInstruction i) =
    let maxDur     = maxOrZero $ map (occupation . usage) (iUsages rm ti)
        useLats    = operandLats $ fst $ oif i
        defLats    = operandLats $ snd $ oif i
        maxOperLat = maxOrZero useLats + maxOrZero defLats
        ii         = fromJust $ elemIndex ti (oInstructions o)
        outgoing   = [dist !! ii | (p, _, dist) <- deps, oId o == p]
        incoming   = [dist | (_, s, dist) <- deps, oId o == s]
        maxDist    = maxOrZero $ catMaybes $ concat incoming ++ outgoing
        maxMinLive = if any isModelOperand (oDefs o) then
                       maximum [minLiveOfDef oif t o
                                 | t <- concatMap extractTemps $ oDefs o]
                     else 0
    in Just $ maximum [maxDur, maxOperLat, maxDist, maxMinLive]

operandLats oifs = [l | TemporaryInfo {oiLatency = l} <- oifs]

maxOrZero = maybeMax 0

maxCost (rm, oif, deps) code =
  let maxc    = map (computeMaxC (rm, oif)) (zip code deps)
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

minLiveOfDef oif t o
  | isIn o      = 1
  | isFun  o    = 1
  | isVirtual o = 0
  | otherwise =
      let lats = [latOf t (tempLatencies oif o i)
                   | TargetInstruction i <- oInstructions o]
      in if any ((==) 0) lats then 0 else 1 :: Latency

latOf t p2l = snd $ fromJust $ find (\(p, _) -> t `elem` extractTemps p) p2l
