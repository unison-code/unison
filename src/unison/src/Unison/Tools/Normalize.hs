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
module Unison.Tools.Normalize (run) where

import Unison
import Unison.Driver
import Unison.Target.API
import Unison.Construction.BuildFunction
import Control.Monad

import qualified MachineIR as MIR

import MachineIR.Transformations.LiftBlockFreqs
import MachineIR.Transformations.RunPreProcess
import MachineIR.Transformations.LiftJumpTables
import MachineIR.Transformations.SimplifyFallthroughs
import MachineIR.Transformations.SplitTerminators
import MachineIR.Transformations.RenameMachineBlocks
import MachineIR.Transformations.DropUnsupportedPseudos
import MachineIR.Transformations.RunPostProcess
import MachineIR.Transformations.PrepareForEmission
import Unison.Transformations.FinalizeOperations
import Unison.Transformations.EstimateFrequency
import qualified Unison.Transformations.NormalizeFrequency as NF

run (estimateFreq, simplifyControlFlow, debug, normMirFile) mir target =
  let mf  = fromSingleton $ MIR.parse mir
      (mf0, partialPreMfs) =
            applyTransformations
            (mirPreTransformations (estimateFreq, simplifyControlFlow))
            target mf
      ff  = buildFunction target mf0
      (f, partialFs) =
            applyTransformations
            (uniTransformations estimateFreq)
            target ff
      mf1 = toMachineFunction f
      (mf2, partialPostMfs) =
            applyTransformations
            mirPostTransformations
            target mf1
  in do
     when debug $
          putStr (toPlainText (partialPreMfs ++ partialFs ++ partialPostMfs))
     emitOutput normMirFile (show mf2)

mirPreTransformations :: (Eq i, Read i, Read r, Eq r) =>
    (Bool, Bool) -> [(MIR.MachineFunction i r -> TargetWithOptions i r rc s ->
    MIR.MachineFunction i r, String, Bool)]
mirPreTransformations (estimateFreq, simplifyControlFlow) =
  [(liftBlockFreqs, "liftBlockFreqs", True),
   (liftJumpTables, "liftJumpTables", True),
   (runPreProcess, "runPreProcess", True),
   (simplifyFallthroughs False, "simplifyFallthroughs", simplifyControlFlow),
   (splitTerminators estimateFreq, "splitTerminators", True),
   (renameMachineBlocks, "renameMachineBlocks", True),
   (dropUnsupportedPseudos, "dropUnsupportedPseudos", True)]

uniTransformations estimateFreq =
  [(estimateFrequency, "estimateFrequency", estimateFreq),
   (NF.normalizeFrequency, "normalizeFrequency", True),
   (finalizeOperations, "finalizeOperations", True)]

mirPostTransformations :: (Eq i, Read i, Read r, Eq r) =>
    [(MIR.MachineFunction i r -> TargetWithOptions i r rc s ->
    MIR.MachineFunction i r, String, Bool)]
mirPostTransformations =
  [(runPostProcess, "runPostProcess", True),
   (prepareForEmission, "prepareForEmission", True)]
