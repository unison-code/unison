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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Unison.Tools.Export (run) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import Control.Monad

import Unison
import Unison.Driver
import Unison.Parser

import Unison.Transformations.FinalizeOperations
import Unison.Transformations.UnbundleSingletons
import Unison.Transformations.RunTargetTransforms
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
import Unison.Tools.Export.RunPostProcess
import Unison.Tools.Export.CleanNops

import Data.Maybe

-- Functions to map JSON into function elements

instance FromJSON RegisterAtom where
    parseJSON (Number a) = return (RegisterAtom (round a))
    parseJSON _          = mzero

run (removeReds, keepNops, baseFile, tight, debug, outJsonFile, unisonMirFile)
  extUni target =
  do outJson <- readFile outJsonFile
     baseMir <- maybeReadFile baseFile
     let f   = parse target extUni
         sol = parseSolution outJson
         (f', partialFs) =
           applyTransformations
           (synthesizerTransformations
            (fromJust sol) (removeReds, keepNops, tight))
           target f
         mf1 = toMachineFunction f'
         mf2 = runPostProcess mf1 target
         mf3 = cleanNops mf2 target
         mf4 = prepareForEmission mf3 target
         mf5 = case baseMir of
                 (Just base) -> base
                 Nothing     -> ""
     when (debug && (isJust sol)) $
          do putStr (toPlainText partialFs)
             putStr $ toPlainText $
               [("toMachineFunction", Just $ showSimple mf1),
                ("runPostProcess", Just $ showSimple mf2),
                ("cleanNops", Just $ showSimple mf3),
                ("prepareForEmission", Just $ showSimple mf4)]
     when (isJust sol) $
          emitOutput unisonMirFile (show mf4)
     when (not $ isJust sol) $
          emitOutput unisonMirFile mf5

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

synthesizerTransformations (cycles, instructions, registers, temporaries)
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
