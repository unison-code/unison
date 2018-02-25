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
{-# LANGUAGE RecordWildCards #-}
module Unison.Tools.Uni (mainWithTargets) where

import Data.Maybe
import System.Environment
import System.Console.CmdArgs

import Common.Util
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
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 do _ <- Import.run
                         (estimateFreq, simplifyControlFlow, noCC, noReserved,
                          maxBlockSize, implementFrames, rematType, function,
                          goal, mirVersion, sizeThreshold, inFile, debug,
                          intermediate, lint, lintPragma, outFile)
                         input (target, targetOption)
                    return ()
    Linearize{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Linearize.run
                 (inFile, debug, intermediate, lint, lintPragma, outFile)
                 input (target, targetOption)
    Extend{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Extend.run
                 (inFile, debug, intermediate, lint, lintPragma, outFile)
                 input (target, targetOption)
    Augment{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Augment.run
                 (implementFrames, noCross, oldModel, expandCopies,
                  rematType,
                  inFile, debug, intermediate, lint, lintPragma, outFile)
                 input (target, targetOption)
    Model{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Model.run
                 (baseFile, scaleFreq, oldModel, applyBaseFile,
                  tightPressureBound, strictlyBetter, unsatisfiable, noCC,
                  outFile)
                 input (target, targetOption)
    Export{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Export.run
                 (removeReds, keepNops, baseFile, tightPressureBound, mirVersion,
                  debug, fromJust solFile, outFile)
                 input (target, targetOption)
    Analyze{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Analyze.run
                 (goals, estimateFreq, simulateStalls, modelCost, boundFile,
                  boundGoal, mirVersion, inFile, debug, intermediate, outFile)
                 input (target, targetOption)
    Normalize{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Normalize.run
                 (estimateFreq, simplifyControlFlow, mirVersion, debug, outFile)
                 input (target, targetOption)
    Lint{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Lint.run rawArgs
                  input (target, targetOption)
    Count{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Count.run
                 (singleRow, inFile, outFile)
                 input (target, targetOption)
    Legalize{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Legalize.run
                 outFile
                 input (target, targetOption)
    Plot{..} ->
        do input <- strictReadFile inFile
           case pickTarget targetName targets of
             (Any target) ->
                 Plot.run
                 (genBcfg, genIcfg, genBdt, genDg, genCg, genSg, genOg, genPg,
                  genPpg, graphBlock, simpleGraph, inFile)
                 input (target, targetOption)
    Run{..} ->
      case pickTarget targetName targets of
       (Any target) -> do
         Run.run
           (estimateFreq, simplifyControlFlow, noCC, noReserved, maxBlockSize, implementFrames,
            function, goal, noCross, oldModel, expandCopies, rematType,
            baseFile, scaleFreq, applyBaseFile, tightPressureBound,
            strictlyBetter, unsatisfiable, removeReds, keepNops, solverFlags, mirVersion,
            inFile, debug, verbose, intermediate, lint, outFile, outTemp,
            presolver, solver, sizeThreshold)
           (target, targetOption)
         return ()
