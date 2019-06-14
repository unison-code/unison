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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Unison.Tools.Export (run) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import Control.Monad

import MachineIR
import Unison
import Unison.Driver
import Unison.Parser
import Unison.Target.API

import Unison.Transformations.FinalizeOperations
import Unison.Transformations.UnbundleSingletons
import Unison.Transformations.RunTargetTransforms
import MachineIR.Transformations.RunPostProcess
import MachineIR.Transformations.SimplifyFallthroughs
import MachineIR.Transformations.RenameMachineBlocks
import MachineIR.Transformations.AddImplicitRegs
import MachineIR.Transformations.PrepareForEmission

import Unison.Tools.Export.SelectTemporaries
import Unison.Tools.Export.AssignRegisters
import Unison.Tools.Export.RemoveRedundancies
import Unison.Tools.Export.BundleOperations
import Unison.Tools.Export.SelectInstructions
import Unison.Tools.Export.LiftFrameObjects
import Unison.Tools.Export.ComputeFrameOffsets
import Unison.Tools.Export.LowerFrameSize
import Unison.Tools.Export.DirectFrame
import Unison.Tools.Export.LowerFrameIndices
import Unison.Tools.Export.RemoveNops
import Unison.Tools.Export.CleanNops

import Data.Maybe

-- Functions to map JSON into function elements

instance FromJSON RegisterAtom where
    parseJSON (Number a) = return (RegisterAtom (round a))
    parseJSON _          = mzero

run (removeReds, keepNops, baseFile, tight, mirVersion, debug, outJsonFile,
     unisonMirFile)
  extUni target =
  do outJson <- strictReadFile outJsonFile
     baseMir <- maybeStrictReadFile baseFile
     let f   = Unison.Parser.parse target extUni
         sol = parseSolution outJson
         (f', partialFs) =
           applyTransformations
           (uniTransformations (fromJust sol) (removeReds, keepNops, tight))
           target f
         mf = toMachineFunction f'
         (mf', partialMfs) =
           applyTransformations (mirTransformations mirVersion) target mf
         mfBase = case baseMir of
                 (Just base) -> base
                 Nothing     -> ""
     when (debug && (isJust sol)) $
          putStr (toPlainText (partialFs ++ partialMfs))
     when (isJust sol) $
          emitOutput unisonMirFile (show mf')
     when (not $ isJust sol) $
          emitOutput unisonMirFile mfBase

parseSolution json =
    let sol          = case decode (BSL.pack json) of
                       Nothing -> error ("error parsing JSON input")
                       Just (Object s) -> s
        cycles       = sol HM.! "cycles"
        instructions = sol HM.! "instructions"
        registers    = sol HM.! "registers"
        temporaries  = sol HM.! "temporaries"
        has_sol      = sol HM.! "has_solution"
    in if (solutionFromJson has_sol :: Bool) then
           Just (solutionFromJson cycles       :: [Integer],
                 solutionFromJson instructions :: [InstructionId],
                 solutionFromJson registers    :: [RegisterAtom],
                 solutionFromJson temporaries  :: [TemporaryId])
       else Nothing

solutionFromJson object =
    case fromJSON object of
      Error e -> error ("error converting JSON input:\n" ++ show e)
      Success s -> s

uniTransformations (cycles, instructions, registers, temporaries)
                               (removeReds, keepNops, tight) =
    [(assignRegisters tight registers, "assignRegisters", True),
     (selectTemporaries temporaries, "selectTemporaries", True),
     (selectInstructions instructions, "selectInstructions", True),
     (runTargetTransforms ExportPreOffs, "runTargetTransforms", True),
     (liftFrameObjects, "liftFrameObjects", True),
     (computeFrameOffsets, "computeFrameOffsets", True),
     (runTargetTransforms ExportPostOffs, "runTargetTransforms", True),
     (lowerFrameSize, "lowerFrameSize", True),
     (directFrame, "directFrame", True),
     (bundleOperations cycles, "bundleOperations", True),
     (removeRedundancies, "removeRedundancies", removeReds),
     (runTargetTransforms ExportPreLow, "runTargetTransforms", True),
     (lowerFrameIndices, "lowerFrameIndices", True),
     (finalizeOperations, "finalizeOperations", True),
     (removeNops, "removeNops", not keepNops),
     (unbundleSingletons, "unbundleSingletons", True)]

mirTransformations :: (Show i, Eq i, Read i, Show r, Read r, Eq r) =>
  MachineIRVersion ->
  [(MachineFunction i r -> TargetWithOptions i r rc s -> MachineFunction i r,
    String, Bool)]
mirTransformations mirVersion =
  [(simplifyFallthroughs True, "simplifyFallthroughs", True),
   (renameMachineBlocks, "renameMachineBlocks", True),
   (runPostProcess, "runPostProcess", True),
   (addImplicitRegs, "addImplicitRegs", True),
   (cleanNops, "cleanNops", True),
   (prepareForEmission mirVersion, "prepareForEmission", True)]
