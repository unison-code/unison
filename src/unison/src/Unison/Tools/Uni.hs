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
{-# LANGUAGE RecordWildCards #-}
module Unison.Tools.Uni (mainWithTargets) where

import Data.Maybe
import System.Environment
import System.Console.CmdArgs

import Unison.Driver
import Unison.Target.API (Any(..), TargetDescription)
import Unison.Tools.UniArgs

import qualified Unison.Tools.Import as Import
import qualified Unison.Tools.Linearize as Linearize
import qualified Unison.Tools.Extend as Extend
import qualified Unison.Tools.Augment as Augment
import qualified Unison.Tools.Model as Model
import qualified Unison.Tools.Export as Export
import qualified Unison.Tools.Analyze as Analyze
import qualified Unison.Tools.Normalize as Normalize
import qualified Unison.Tools.Lint as Lint
import qualified Unison.Tools.Count as Count
import qualified Unison.Tools.Legalize as Legalize
import qualified Unison.Tools.Plot as Plot
import qualified Unison.Tools.Run as Run

mainWithTargets :: [(String, Any TargetDescription)] -> IO ()
mainWithTargets targets = do
  rawArgs <- getArgs
  args <- cmdArgsRun uniArgs
  case args of
    Import{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Import.run
                 (estimateFreq, noCC, noReserved, maxBlockSize, implementFrames,
                  function,
                  inFile, debug, intermediate, lint, outFile)
                 input (target, targetOption)
    Linearize{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Linearize.run
                 (inFile, debug, intermediate, lint, outFile)
                 input (target, targetOption)
    Extend{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Extend.run
                 (inFile, debug, intermediate, lint, outFile)
                 input (target, targetOption)
    Augment{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Augment.run
                 (implementFrames, noCross, oldModel, expandCopies,
                  rematerialize,
                  inFile, debug, intermediate, lint, outFile)
                 input (target, targetOption)
    Model{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Model.run
                 (baseFile, scaleFreq, oldModel, optimizeDynamic,
                  optimizeResource, applyBaseFile, tightPressureBound,
                  strictlyBetter, outFile)
                 input (target, targetOption)
    Export{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Export.run
                 (removeReds, keepNops, baseFile, tightPressureBound, debug,
                  fromJust solFile, outFile)
                 input (target, targetOption)
    Analyze{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Analyze.run
                 (fromJust goals, estimateFreq, simulateStalls, modelCost,
                  inFile, debug, intermediate, outFile)
                 input (target, targetOption)
    Normalize{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Normalize.run
                 (estimateFreq, debug, outFile)
                 input (target, targetOption)
    Lint{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Lint.run rawArgs
                  input (target, targetOption)
    Count{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Count.run
                 (singleRow, inFile, outFile)
                 input (target, targetOption)
    Legalize{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Legalize.run
                 outFile
                 input (target, targetOption)
    Plot{..} ->
        do input <- readFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Plot.run
                 (genBcfg, genIcfg, genBdt, genDg, genCg, genSg, genOg, genPg,
                  graphBlock, simpleGraph, inFile)
                 input (target, targetOption)
    Run{..} ->
      case pickTarget targetName targets of
       (Any target) -> do
         Run.run
           (estimateFreq, noCC, noReserved, maxBlockSize, implementFrames,
            function, noCross, oldModel, expandCopies, rematerialize, baseFile,
            scaleFreq, optimizeDynamic, optimizeResource, applyBaseFile,
            tightPressureBound, strictlyBetter, removeReds, keepNops,
            inFile, debug, verbose, intermediate, lint, outFile, outTemp,
            presolver, solver)
           (target, targetOption)
         return ()
