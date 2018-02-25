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
module Unison.Tools.Import (run) where

import Data.List
import Data.Maybe
import System.FilePath
import Control.Monad

import Unison.Base
import Unison.Driver
import Unison.Tools.UniArgs
import Unison.Tools.Lint (invokeLint)

import qualified MachineIR as MachineIR

import Unison.Construction.AddDelimiters
import Unison.Construction.LiftGoal
import Unison.Construction.BuildFunction

import MachineIR.Transformations.LiftBlockFreqs
import MachineIR.Transformations.LiftCustomProperties
import MachineIR.Transformations.LiftJumpTables
import MachineIR.Transformations.SimplifyFallthroughs
import MachineIR.Transformations.SplitTerminators
import MachineIR.Transformations.RenameMachineBlocks
import MachineIR.Transformations.DropUnsupportedPseudos
import MachineIR.Transformations.RunPreProcess

import Unison.Transformations.RenameBlocks
import Unison.Transformations.PostponeBranches
import Unison.Transformations.RenameTemps
import Unison.Transformations.SortGlobalTemps
import Unison.Transformations.RenameOperations
import Unison.Transformations.EstimateFrequency
import Unison.Transformations.NormalizeFrequency
import Unison.Transformations.AddPragmas
import Unison.Transformations.RunTargetTransforms

import Unison.Tools.Import.DropDebugLocations
import Unison.Tools.Import.NormalizePhis
import Unison.Tools.Import.LiftBranchPredictions
import Unison.Tools.Import.LiftMemoryPartitions
import Unison.Tools.Import.LiftMachineUndefs
import Unison.Tools.Import.ExtractSubRegs
import Unison.Tools.Import.LowerInsertSubRegs
import Unison.Tools.Import.LowerSubRegVirtuals

import Unison.Tools.Import.RemoveUnreachableBlocks
import Unison.Tools.Import.CorrectDoubleBranches
import Unison.Tools.Import.AdjustPhiLabels
import Unison.Tools.Import.SimplifyCombinations
import Unison.Tools.Import.RemoveUselessVirtuals
import Unison.Tools.Import.RelocateDefines
import Unison.Tools.Import.ExtractCallRegs
import Unison.Tools.Import.LiftRegs
import Unison.Tools.Import.EnforceCallerSaved
import Unison.Tools.Import.LiftUndefRegs
import Unison.Tools.Import.KillUnusedTemps
import Unison.Tools.Import.ExtractRegs
import Unison.Tools.Import.EnforceCalleeSaved
import Unison.Tools.Import.ReserveRegs
import Unison.Tools.Import.ImplementFrameOperations
import Unison.Tools.Import.FoldCopies
import Unison.Tools.Import.SplitBlocks
import Unison.Tools.Import.RepairCSSA
import Unison.Tools.Import.AdvancePhis
import Unison.Tools.Import.TagRemats

run (estimateFreq, simplifyControlFlow, noCC, noReserved, maxBlockSize,
     implementFrames, rematType, function, goal, mirVersion, sizeThreshold,
     mirFile, debug, intermediate, lint, lintPragma, uniFile) mir target =
    let mf = selectFunction function $ MachineIR.parse mirVersion mir
        (mf', partialMfs) =
            applyTransformations
            (mirTransformations (estimateFreq, simplifyControlFlow))
            target mf
        ff = buildFunction target mf'
        (f, partialFs) =
            applyTransformations
            (uniTransformations (goal, noCC, noReserved, maxBlockSize,
                                 estimateFreq, implementFrames, rematType,
                                 lintPragma))
            target ff
        baseName = takeBaseName mirFile
    in case overThreshold sizeThreshold mf of
        True  -> do return Nothing
        False -> do when debug $
                         putStr (toPlainText (partialMfs ++ partialFs))
                    when intermediate $
                         mapM_ (writeIntermediateFile "uni" baseName) partialFs
                    emitOutput uniFile (show f)
                    when lint $
                         invokeLint f target
                    return (Just uniFile)

mirTransformations (estimateFreq, simplifyControlFlow) =
    [(dropDebugLocations, "dropDebugLocations", True),
     (normalizePhis, "normalizePhis", True),
     (liftBlockFreqs, "liftBlockFreqs", True),
     (liftCustomProperties, "liftCustomProperties", True),
     (liftBranchPredictions, "liftBranchPredictions", True),
     (liftJumpTables, "liftJumpTables", True),
     (liftMemoryPartitions, "liftMemoryPartitions", True),
     (simplifyFallthroughs False, "simplifyFallthroughs", simplifyControlFlow),
     (renameMachineBlocks, "renameMachineBlocks", True),
     (splitTerminators estimateFreq, "splitTerminators", True),
     (renameMachineBlocks, "renameMachineBlocks", True),
     (dropUnsupportedPseudos, "dropUnsupportedPseudos", True),
     (liftMachineUndefs, "liftMachineUndefs", True),
     (extractSubRegs, "extractSubRegs", True),
     (lowerInsertSubRegs, "lowerInsertSubRegs", True),
     (lowerSubRegVirtuals, "lowerSubRegVirtuals", True),
     (runPreProcess, "runPreProcess", True)]

uniTransformations (goal, noCC, noReserved, maxBlockSize, estimateFreq,
                    implementFrames, rematType, lintPragma) =
    [(liftGoal goal, "liftGoal", True),
     (addDelimiters, "addDelimiters", True),
     (postponeBranches, "postponeBranches", True),
     (removeUnreachableBlocks, "removeUnreachableBlocks", True),
     (renameBlocks, "renameBlocks", True),
     (correctDoubleBranches, "correctDoubleBranches", True),
     (adjustPhiLabels, "adjustPhiLabels", True),
     (simplifyCombinations, "simplifyCombinations", True),
     (removeUselessVirtuals, "removeUselessVirtuals", True),
     (relocateDefines, "relocateDefines", True),
     (runTargetTransforms ImportPreLift, "runTargetTransforms", True),
     (extractCallRegs, "extractCallRegs", True),
     (liftRegs, "liftRegs", True),
     (runTargetTransforms ImportPostLift, "runTargetTransforms", True),
     (enforceCallerSaved, "enforceCallerSaved", not noCC),
     (enforceCalleeSaved, "enforceCalleeSaved", not noCC),
     (reserveRegs, "reserveRegs", not noReserved),
     (implementFrameOperations implementFrames, "implementFrameOperations", True),
     (liftUndefRegs, "liftUndefRegs", True),
     (killUnusedTemps, "killUnusedTemps", True),
     (extractRegs, "extractRegs", True),
     (foldCopies, "foldCopies", True),
     (splitBlocks (fromJust maxBlockSize), "splitBlocks", isJust maxBlockSize),
     (renameBlocks, "renameBlocks", True),
     (repairCSSA, "repairCSSA", True),
     (advancePhis, "advancePhis", True),
     (postponeBranches, "postponeBranches", True),
     (renameTemps, "renameTemps", True),
     (sortGlobalTemps, "sortGlobalTemps", True),
     (renameOperations, "renameOperations", True),
     (estimateFrequency, "estimateFrequency", estimateFreq),
     (normalizeFrequency, "normalizeFrequency", True),
     (tagRemats, "tagRemats", rematType == CopyRemat),
     (addPragmas importPragmas, "addPragmas", lintPragma)]

importPragmas =
    [("lint",
      "--nomustconflicts=false " ++
      "--nocostoverflow=false " ++
      "--nocomponentconflicts=false")]

selectFunction Nothing [mf] = mf
selectFunction (Just name) mfs =
  case find (\mf -> MachineIR.mfName mf == name) mfs of
    Just mf -> mf
    Nothing -> error ("could not find specified MIR function " ++ show name)

overThreshold Nothing _ = False
overThreshold (Just max) mf =
  toInteger (length (MachineIR.flattenMachineFunction mf)) > max
