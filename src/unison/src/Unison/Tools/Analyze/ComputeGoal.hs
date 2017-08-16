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
module Unison.Tools.Analyze.ComputeGoal (computeGoal) where

import Data.Maybe
import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Target.Query
import Unison.Analysis.MakespanBounds

computeGoal modelCost factor target (gl, Function {fCode = code}) =
  let uf   = usages target
      cf   = capacityMap target

      b2f  = M.fromList $
             map (\b -> (bLab b, scaleDown factor (blockFreq b))) code
      gs   = goal modelCost (uf, cf) b2f code gl
  in if any isNothing (map (aFreq . bAs) code) then
       error ("frequency estimations are not given, use the flag -f to let the analyzer estimate them")
     else gs

goal modelCost aux b2f code (StaticGoal o) =
    goalOb modelCost aux (M.map (const 1) b2f) code o
goal modelCost aux b2f code (DynamicGoal o) =
    goalOb modelCost aux b2f code o

goalOb modelCost aux b2f code go =
    sumMap (goalObInBlock modelCost aux b2f go) code

goalObInBlock modelCost aux b2f go Block {bLab = l, bCode = code} =
    (b2f M.! l) * sumMap (goalObInOpr modelCost aux go) code

goalObInOpr modelCost _ Cycles o
    | modelCost && (isIn o || isFun o) = 1
    | isVirtual o = 0
    | otherwise = 1
goalObInOpr modelCost aux ru @ (ResourceUsage _) Bundle {bundleOs = os} =
    sumMap (goalObInOpr modelCost aux ru) os
goalObInOpr modelCost (uf, cf) (ResourceUsage r) o
    | modelCost && isBarrier o = cf M.! r
    | isVirtual o = 0
    | otherwise =
        let us = uf $ targetInst $ oInstructions o
        in case lookupBy r resource us of
             Nothing   -> 0
             (Just ru) -> units ru * occupation ru

sumMap f = sum . map f
