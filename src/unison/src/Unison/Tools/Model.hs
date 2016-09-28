{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

Contributing authors:
  Patric Hedlin <patric.hedlin@ericsson.com>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Unison.Tools.Model (run) where

import qualified Data.Map as M
import Data.List
import Data.Ord
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Lazy.Builder as DTB
import Data.Scientific

import Common.Util
import Unison.Driver
import Unison.Target.API
import Unison.Parser

import qualified MachineIR as MIR

import Unison.Tools.Model.Definitions
import Unison.Tools.Model.Presolver

import Unison
import Unison.Target.Query
import Unison.Target.RegisterArray
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.DG as DG
import Unison.Analysis.TemporaryType
import Unison.Analysis.MakespanBounds

import qualified Unison.Tools.Analyze as Analyze

import qualified Unison.Tools.Model.InstructionScheduling as IS
import qualified Unison.Tools.Model.RegisterAllocation as RA

run (baseFile, scaleFreq, oldModel, optimizeDynamic, optimizeResource,
     applyBaseFile, tightPressureBound, strictlyBetter, jsonFile)
    extUni target =
  do baseMir <- maybeReadFile baseFile
     let f    = parse target extUni
         base = maybeNothing applyBaseFile baseMir
         aux  = auxiliarDataStructures target tightPressureBound base f
         ps   = modeler scaleFreq aux target f
         ps'  = optimization strictlyBetter scaleFreq optimizeDynamic
                optimizeResource aux target f ps
         ps'' = presolver oldModel aux target f ps'
     emitOutput jsonFile ((BSL.unpack (encodePretty' jsonConfig ps'')))

jsonConfig = defConfig {confNumFormat = Custom showInteger}

modeler scaleFreq aux target f =
  toJSON (M.fromList (IS.parameters scaleFreq aux f target ++
                      RA.parameters aux f target))

auxiliarDataStructures target tight baseMir f @ Function {fCode = code} =
  let rwlf  = readWriteLatency target
      oif   = operandInfo target
      rm    = resourceManager target
      fCode = sortBy (comparing oId) (flatten code)
      ra    = mkRegisterArray target 0
      cg    = CG.fromFunction f
      dgs   = map (DG.fromBlock rwlf rm oif) code
      t2w   = tempWidths ra oif fCode cg
      inf   = maxTempWidth tight code t2w
      ra'   = mkRegisterArray target inf
  in (cg, dgs, t2w, ra', baseMir)

optimization strictlyBetter scaleFreq optimizeDynamic optimizeResource aux
  target f ps =
    let ops = toJSON (M.fromList (optimizationParameters
                                  strictlyBetter scaleFreq optimizeDynamic
                                  optimizeResource
                                  aux target f))
    in unionMaps ps ops

optimizationParameters strictlyBetter scaleFreq optimizeDynamic optimizeResource
                       (_, dgs, _, _, baseMir)
                       target Function {fCode = code} =
    let rm   = resourceManager target
        cf   = capacityMap target
        r2id = M.fromList [(resName (res ir), resId ir) | ir <- iResources rm]
        gl   = mkGoal optimizeDynamic optimizeResource
        od   = isDynamic gl :: Bool
        or   = optResource r2id gl :: ResourceId
        maxf = case baseMir of
                 (Just mir) ->
                     let mf = fromSingleton $ MIR.parse mir
                     in maximumCost strictlyBetter scaleFreq cf gl (mir, mf) dgs
                        target code
                 Nothing -> maxInt
    in
      [
      -- Parameters related to the objective function

      -- whether to use block frequencies as weight
      ("optimize_dynamic", toJSON od),

      -- resource whose consumption is to be optimized
      ("optimize_resource", toJSON or),

      -- upper bound of the objective
      ("maxf", toJSON maxf)
      ]

mkGoal optimizeDynamic optimizeResource =
    let go = mkGoalObject optimizeResource
    in if optimizeDynamic then DynamicGoal go else StaticGoal go

mkGoalObject "cycles" = Cycles
mkGoalObject r = ResourceUsage (read r)

isDynamic (DynamicGoal _) = True
isDynamic _ = False

optResource r2id (DynamicGoal r) = optResource' r2id r
optResource r2id (StaticGoal r)  = optResource' r2id r

optResource' _ Cycles = -1
optResource' r2id (ResourceUsage r) = r2id M.! r

maximumCost :: (Eq i, Show i, Read i, Ord r, Show r, Read r, Ord rc, Show rc,
                Ord s) =>
               Bool -> Bool -> M.Map s Integer -> Goal s ->
               (String, MIR.MachineFunction i r) ->
               [DGraph i r] -> TargetWithOptions i r rc s -> [Block i r] ->
               Integer
maximumCost strictlyBetter scaleFreq cf gl (mir, mf) dgs target code =
    let rm     = resourceManager target
        oif    = operandInfo target
        gl'    = [gl]
        bbs    = map MIR.machineBlockFreq (MIR.mfBlocks mf)
        fbs    = map blockFreq code
        factor = if scaleFreq then scaleFactor (rm, oif, dgs) code else 1.0
        nf     = sort . map (scaleDown factor)
        ([baseCost], _) = Analyze.analyze (False, True, True)
                          factor gl' mir target
        baseCost' = baseCost + compensation cf gl (nf fbs) (nf bbs)
        maxCost = if strictlyBetter then baseCost' - 1 else baseCost'
    in maxCost

-- Compensate from extra blocks in either side
-- TODO: handle case where freqs are scaled down!
compensation cf gl fbs bbs
    | fbs == bbs     = 0
    | subset bbs fbs =
        let diff = difference fbs bbs
            fs   = [if isDynamic gl then f else 1 | f <- diff]
        in sum [f * inOverhead cf (goalObject gl) | f <- fs]
    | otherwise = - compensation cf gl bbs fbs

inOverhead _ Cycles = 1
inOverhead cf (ResourceUsage r) = cf M.! r

subset l1 l2 = length (difference l2 l1) > 0

difference l1 l2 = l1 \\ l2

goalObject (DynamicGoal o) = o
goalObject (StaticGoal o)  = o

showInteger i =
  case floatingOrInteger i of
   Right i' -> DTB.fromString (show (toInteger i'))
   Left r -> error ("expecting integer but got " ++ show r)
