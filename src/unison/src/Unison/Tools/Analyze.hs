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

import Data.Aeson
import qualified Data.Map as M
import System.FilePath
import Control.Monad
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import Data.List.Split
import Data.String

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
import Unison.Tools.Analyze.FilterOverhead
import Unison.Tools.Analyze.ComputeGoal

run (goals, estimateFreq, simulateStalls, modelCost, boundFile, boundGoal,
     mirVersion, mirFile, debug, intermediate, jsonFile) mir target =
  do inBounds <- maybeStrictReadFile boundFile
     let af = \gs mc oo ->
               analyze (estimateFreq, simulateStalls, mirVersion, mc, oo)
               1.0 gs mir target
         -- compute requested costs
         sgoals   = splitOn "," goals
         gs       = map (lowerGoal . read) sgoals
         (rs, partialFs) = af gs modelCost False
         results  = [(g, toJSON v) | (g, v) <- zip sgoals rs]
         -- compute lower bound
         bg       = lowerGoal $ read boundGoal
         ([overhead], _) = af [bg] (not modelCost) True
         lbs      = fmap (map (\lb -> lb - overhead) . parseLowerBound) inBounds
         bound    = case lbs of
                     Just [lb] ->
                       let ([solCost], _) = af [bg] modelCost False
                           -- the input lower bound of a proven sol. is 'maxint'
                           lb' = min lb solCost
                       in [("lower_bound", toJSON lb')]
                     _ -> []
         -- merge both results
         ps       = toJSON $ M.fromList (results ++ bound)
         baseName = takeBaseName mirFile
     when debug $ putStr (toPlainText partialFs)
     when intermediate $
       mapM_ (writeIntermediateFile "a" baseName) partialFs
     emitOutput jsonFile (BSL.unpack (encodePretty ps))

analyze (estimateFreq, simulateStalls, mirVersion, modelCost, overheadOnly)
  factor gl mir target =
  let mf  = fromSingleton $ MIR.parse mirVersion mir
      mf' = MIR.runMachineTransformations (preProcess target) mf
      ff  = buildFunction target mf'
      (f, partialFs) =
        applyTransformations
        (analyzerGeneralTransformations (estimateFreq, modelCost, overheadOnly))
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

analyzerGeneralTransformations (estimateFreq, modelCost, overheadOnly) =
    [(addDelimiters, "addDelimiters", True),
     (estimateFrequency, "estimateFrequency", estimateFreq),
     (insertFuns, "insertFuns", modelCost),
     (filterOverhead, "filterOverhead", overheadOnly)]

analyzerCycleTransformations simulateStalls =
    [(insertNops, "insertNops", simulateStalls),
     (unbundleSingletons, "unbundleSingletons", True)]

parseLowerBound json =
  let bounds = case decode (BSL.pack json) of
                Nothing -> error ("error parsing bounds file")
                Just (Object s) -> s
  in boundFromJson (bounds HM.! (fromString "lower_bound")) :: [Integer]

boundFromJson object =
  case fromJSON object of
   Error e -> error ("error converting JSON input:\n" ++ show e)
   Success s -> s
