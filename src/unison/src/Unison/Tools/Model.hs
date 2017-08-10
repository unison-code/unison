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

run (baseFile, scaleFreq, oldModel, applyBaseFile, tightPressureBound,
     strictlyBetter, unsatisfiable, jsonFile)
    extUni target =
  do baseMir <- maybeReadFile baseFile
     let f    = parse target extUni
         base = maybeNothing applyBaseFile baseMir
         aux  = auxiliarDataStructures target tightPressureBound base f
         ps   = modeler scaleFreq aux target f
         ps'  = optimization (strictlyBetter, unsatisfiable, scaleFreq)
                aux target f ps
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
      deps  = map DG.dependencies dgs
      t2w   = tempWidths ra oif fCode cg
      inf   = maxTempWidth tight code t2w
      ra'   = mkRegisterArray target inf
  in (cg, dgs, deps, t2w, ra', baseMir)

optimization flags aux target f ps =
    let ops = toJSON (M.fromList (optimizationParameters flags aux target f))
    in unionMaps ps ops

optimizationParameters (strictlyBetter, unsatisfiable, scaleFreq)
  (_, _, deps, _, _, baseMir) target Function {fCode = code, fGoal = goal} =
    let rm    = resourceManager target
        cf    = capacityMap target
        r2id  = M.fromList [(resName (res ir), resId ir) | ir <- iResources rm]
        gl    = mkGoal goal
        od    = map isDynamic gl :: [Bool]
        or    = map (optResource r2id) gl :: [ResourceId]
        maxf0 = case baseMir of
                 (Just mir) ->
                     let mf = fromSingleton $ MIR.parse mir
                         mc = maximumCost scaleFreq cf
                         mx = map (\g -> mc g (mir, mf) deps target code) gl
                     in if strictlyBetter then decrementLast mx else mx
                 Nothing -> replicate (length gl) maxInt
        maxf  = if unsatisfiable then replicate (length gl) 0 else maxf0
    in
      [
      -- Parameters related to the objective function

      -- whether to use block frequencies as weight for the nth objective
      ("optimize_dynamic", toJSON od),

      -- resource whose consumption is to be optimized for the nth objective
      ("optimize_resource", toJSON or),

      -- upper bound of the nth objective
      ("maxf", toJSON maxf)
      ]

decrementLast l = init l ++ [(last l) - 1]

mkGoal [] = error ("optimization goal is missing")
mkGoal goal = map lowerGoal goal

isDynamic (DynamicGoal _) = True
isDynamic _ = False

optResource r2id (DynamicGoal r) = optResource' r2id r
optResource r2id (StaticGoal r)  = optResource' r2id r

optResource' _ Cycles = -1
optResource' r2id (ResourceUsage r) = r2id M.! r

maximumCost :: (Eq i, Show i, Read i, Ord r, Show r, Read r, Ord rc, Show rc,
                Ord s, Show s) =>
               Bool -> M.Map s Integer -> Goal s ->
               (String, MIR.MachineFunction i r) ->
               [[(OperationId, OperationId, [Maybe Latency])]] ->
               TargetWithOptions i r rc s -> [Block i r] ->
               Integer
maximumCost scaleFreq cf gl (mir, mf) deps target code =
    let rm     = resourceManager target
        oif    = operandInfo target
        bbs    = map MIR.machineBlockFreq (MIR.mfBlocks mf)
        fbs    = map blockFreq code
        factor = if scaleFreq then scaleFactor (rm, oif, deps) code else 1.0
        nf     = sort . map (scaleDown factor)
        ([baseCost], _) = Analyze.analyze (False, True, True)
                          factor [gl] mir target
        baseCost' = baseCost + compensation cf gl (nf fbs) (nf bbs)
    in baseCost'

-- Compensate from extra blocks in either side
-- TODO: handle case where freqs are scaled down!
compensation cf gl fbs bbs
    | fbs == bbs     = 0
    | subset bbs fbs =
        let diff = difference fbs bbs
            fs   = [if isDynamic gl then f else 1 | f <- diff]
            go   = goalObject gl
        in sum [f * (inOverhead cf go + outOverhead cf go) | f <- fs]
    | otherwise = - compensation cf gl bbs fbs

inOverhead _ Cycles = 1
inOverhead cf (ResourceUsage r) = cf M.! r

outOverhead _ Cycles = 0
outOverhead cf (ResourceUsage r) = cf M.! r

subset l1 l2 = length (difference l2 l1) > 0

difference l1 l2 = l1 \\ l2

goalObject (DynamicGoal o) = o
goalObject (StaticGoal o)  = o

showInteger i =
  case floatingOrInteger i of
   Right i' -> DTB.fromString (show (toInteger i'))
   Left r -> error ("expecting integer but got " ++ show r)
