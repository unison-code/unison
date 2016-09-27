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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Unison.Tools.Normalize (run) where

import Unison
import Unison.Driver
import Unison.Target.API
import Unison.Construction.BuildFunction
import Control.Monad

import qualified MachineIR as MIR

import MachineIR.Transformations.LiftJumpTables
import MachineIR.Transformations.SimplifyFallthroughs
import MachineIR.Transformations.SplitTerminators
import MachineIR.Transformations.RenameMachineBlocks
import MachineIR.Transformations.PrepareForEmission
import Unison.Transformations.FinalizeOperations
import Unison.Transformations.EstimateFrequency
import qualified Unison.Transformations.NormalizeFrequency as NF

run (estimateFreq, debug, normMirFile) mir target =
  let mf  = fromSingleton $ MIR.parse mir
      mf0 = liftJumpTables mf target
      mf1 = MIR.runMachineTransformations (preProcess target) mf0
      mf2 = simplifyFallthroughs mf1 target
      mf3 = splitTerminators estimateFreq mf2 target
      mf4 = renameMachineBlocks mf3 target
      f   = buildFunction target mf4
      f1  = if estimateFreq then estimateFrequency f target else f
      f2  = NF.normalizeFrequency f1 target
      f3  = finalizeOperations f2 target
      mf5 = toMachineFunction f3
      mf6 = MIR.runMachineTransformations (postProcess target) mf5
      mf7 = prepareForEmission mf6 target
  in do
     when debug $
          putStr $ toPlainText $
          [("liftJumpTables", Just $ show mf0),
           ("preProcess", Just $ show mf1),
           ("simplifyFallthroughs", Just $ show mf2),
           ("splitTerminators", Just $ show mf3),
           ("renameMachineBlocks", Just $ show mf4),
           ("buildFunction", Just $ show f),
           ("estimateFrequency", Just $ show f1),
           ("normalizeFrequency", Just $ show f2),
           ("finalizeOperations", Just $ show f3),
           ("toMachineFunction", Just $ show mf5),
           ("postProcess", Just $ show mf6),
           ("prepareForEmission", Just $ show mf7)]
     emitOutput normMirFile (show mf7)
