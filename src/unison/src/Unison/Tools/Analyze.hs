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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Unison.Tools.Analyze (run, analyze) where

import qualified Data.Map as M
import System.FilePath
import Control.Monad
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List.Split

import Unison
import Unison.Target.API
import Unison.Driver

import qualified MachineIR as MIR

import Unison.Construction.BuildFunction
import Unison.Construction.AddDelimiters
import Unison.Transformations.EstimateFrequency
import Unison.Transformations.UnbundleSingletons

import Unison.Tools.Analyze.InsertNops
import Unison.Tools.Analyze.InsertFuns
import Unison.Tools.Analyze.ComputeGoal

run (goals, estimateFreq, simulateStalls, modelCost,
     mirFile, debug, intermediate, jsonFile) mir target =
  let sgoals          = splitOn "," goals
      gs              = map (lowerGoal . read) sgoals
      (rs, partialFs) = analyze (estimateFreq, simulateStalls, modelCost)
                        1.0 gs mir target
      results         = zip sgoals rs
      baseName        = takeBaseName mirFile
  in do when debug $
             putStr (toPlainText partialFs)
        when intermediate $
             mapM_ (writeIntermediateFile "a" baseName) partialFs
        emitOutput jsonFile (BSL.unpack (encodePretty (M.fromList results)))

analyze (estimateFreq, simulateStalls, modelCost)
  factor gl mir target =
  let mf  = fromSingleton $ MIR.parse mir
      mf' = MIR.runMachineTransformations (preProcess target) mf
      ff  = buildFunction target mf'
      (f, partialFs) =
        applyTransformations
        (analyzerGeneralTransformations (estimateFreq, modelCost))
        target ff

      (cf, partialFs') =
        applyTransformations
        (analyzerCycleTransformations simulateStalls)
        target f
      rf  = f

      gfs = map (associateFunction (cf, rf)) gl
      gs  = map (computeGoal modelCost factor target) gfs

  in (gs, partialFs ++ partialFs')

associateFunction (cf, _) g @ (DynamicGoal Cycles)            = (g, cf)
associateFunction (cf, _) g @ (StaticGoal Cycles)             = (g, cf)
associateFunction (_, rf) g @ (DynamicGoal (ResourceUsage _)) = (g, rf)
associateFunction (_, rf) g @ (StaticGoal (ResourceUsage _))  = (g, rf)

analyzerGeneralTransformations (estimateFreq, modelCost) =
    [(addDelimiters, "addDelimiters", True),
     (estimateFrequency, "estimateFrequency", estimateFreq),
     (insertFuns, "insertFuns", modelCost)]

analyzerCycleTransformations simulateStalls =
    [(insertNops, "insertNops", simulateStalls),
     (unbundleSingletons, "unbundleSingletons", True)]
