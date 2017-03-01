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
import MachineIR.Transformations.DropUnsupportedPseudos
import MachineIR.Transformations.PrepareForEmission
import Unison.Transformations.FinalizeOperations
import Unison.Transformations.EstimateFrequency
import qualified Unison.Transformations.NormalizeFrequency as NF

run (estimateFreq, simplifyControlFlow, debug, normMirFile) mir target =
  let mf  = fromSingleton $ MIR.parse mir
      mf0 = liftJumpTables mf target
      mf1 = MIR.runMachineTransformations (preProcess target) mf0
      mf2 = if simplifyControlFlow then simplifyFallthroughs mf1 target else mf1
      mf3 = splitTerminators estimateFreq mf2 target
      mf4 = renameMachineBlocks mf3 target
      mf5 = dropUnsupportedPseudos mf4 target
      f   = buildFunction target mf5
      f1  = if estimateFreq then estimateFrequency f target else f
      f2  = NF.normalizeFrequency f1 target
      f3  = finalizeOperations f2 target
      mf6 = toMachineFunction f3
      mf7 = MIR.runMachineTransformations (postProcess target) mf6
      mf8 = prepareForEmission mf7 target
  in do
     when debug $
          putStr $ toPlainText $
          [("liftJumpTables", Just $ showSimple mf0),
           ("preProcess", Just $ showSimple mf1),
           ("simplifyFallthroughs", Just $ showSimple mf2),
           ("splitTerminators", Just $ showSimple mf3),
           ("renameMachineBlocks", Just $ showSimple mf4),
           ("dropUnsupportedPseudos", Just $ showSimple mf5),
           ("buildFunction", Just $ showSimple f),
           ("estimateFrequency", Just $ showSimple f1),
           ("normalizeFrequency", Just $ showSimple f2),
           ("finalizeOperations", Just $ showSimple f3),
           ("toMachineFunction", Just $ showSimple mf6),
           ("postProcess", Just $ showSimple mf7),
           ("prepareForEmission", Just $ showSimple mf8)]
     emitOutput normMirFile (show mf8)
