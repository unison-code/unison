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
      sof  = spillOverhead target

      b2f  = M.fromList $
             map (\b -> (bLab b, scaleDown factor (blockFreq b))) code
      gs   = goal sof modelCost (uf, cf) b2f code gl
  in if any isNothing (map (aFreq . bAs) code) then
       error ("frequency estimations are not given, use the flag -f to let the analyzer estimate them")
     else gs

goal sof modelCost aux b2f code (StaticGoal o) =
    goalOb sof modelCost aux (M.map (const 1) b2f) code o
goal sof modelCost aux b2f code (DynamicGoal o) =
    goalOb sof modelCost aux b2f code o

goalOb sof modelCost aux b2f code go =
    sumMap (goalObInBlock sof modelCost aux b2f go) code

goalObInBlock sof modelCost aux b2f go Block {bLab = l, bCode = code} =
  let f = b2f M.! l
      c = sumMap (goalObInOpr sof modelCost aux go) code
  in f * c

goalObInOpr _ modelCost _ Cycles o
    | modelCost && (isIn o || isFun o) = 1
    | isVirtual o = 0
    | otherwise = 1
goalObInOpr sof modelCost aux ru @ SpillOverhead Bundle {bundleOs = os} =
    sumMap (goalObInOpr sof modelCost aux ru) os
goalObInOpr sof _ _ SpillOverhead o
    | isVirtual o = 0
    | otherwise =
        case sof (targetInst $ oInstructions o, oUses o, oDefs o) of
         Just (_, l) -> l
         Nothing -> 0
goalObInOpr sof modelCost aux ru @ SpillAction Bundle {bundleOs = os} =
    sumMap (goalObInOpr sof modelCost aux ru) os
goalObInOpr sof _ _ SpillAction o
    | isVirtual o = 0
    | otherwise =
        case sof (targetInst $ oInstructions o, oUses o, oDefs o) of
         Just (True, _) -> 1
         _ -> 0
goalObInOpr sof modelCost aux ru @ (ResourceUsage _) Bundle {bundleOs = os} =
    sumMap (goalObInOpr sof modelCost aux ru) os
goalObInOpr _ modelCost (uf, cf) (ResourceUsage r) o
    | modelCost && isBarrier o = cf M.! r
    | isVirtual o = 0
    | otherwise =
        let us = uf $ targetInst $ oInstructions o
        in case lookupBy r resource us of
             Nothing   -> 0
             (Just ru) -> units ru * occupation ru

sumMap f = sum . map f
