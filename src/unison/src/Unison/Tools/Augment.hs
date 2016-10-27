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
module Unison.Tools.Augment (run) where

import System.FilePath
import Control.Monad

import Unison.Base
import Unison.Driver
import Unison.Parser
import Unison.Tools.Lint (invokeLint)

import Unison.Transformations.PostponeBranches
import Unison.Transformations.RenameTemps
import Unison.Transformations.RenameMOperands
import Unison.Transformations.RenameOperations
import Unison.Transformations.CleanPragmas
import Unison.Transformations.AddPragmas
import Unison.Transformations.RunTargetTransforms

import Unison.Tools.Augment.GeneralizeOperands
import Unison.Tools.Augment.GeneralizeCongruences
import Unison.Tools.Augment.AugmentOperands
import Unison.Tools.Augment.MoveUpPacks
import Unison.Tools.Augment.ExpandCopies
import Unison.Tools.Augment.AddRematerialization
import Unison.Tools.Augment.AddPrologueEpilogue
import Unison.Tools.Augment.CleanAttributes
import Unison.Tools.Augment.AddReadWrites
import Unison.Tools.Augment.LiftMemInfo

run (implementFrames, noCross, oldModel, expandCopies', rematerialize,
     uniFile, debug, intermediate, lint, altUniFile) uni target =
    let f = parse target uni
        (altF, partialAltFs) =
            applyTransformations
            (augmenterTransformations (implementFrames, noCross, oldModel,
                                       expandCopies', rematerialize))
            target f
        baseName = takeBaseName uniFile
    in do when debug $
               putStr (toPlainText partialAltFs)
          when intermediate $
               mapM_ (writeIntermediateFile "alt.uni" baseName) partialAltFs
          emitOutput altUniFile (show altF)
          when lint $
               invokeLint altF target

augmenterTransformations (implementFrames, noCross, oldModel, expandCopies',
                          rematerialize) =
    [(generalizeOperands, "generalizeOperands", True),
     (generalizeCongruences, "generalizeCongruences", True),
     (augmentOperands noCross oldModel, "augmentOperands", True),
     (moveUpPacks, "moveUpPacks", True),
     (expandCopies, "expandCopies", expandCopies'),
     (addRematerialization, "addRematerialization", rematerialize),
     (postponeBranches, "postponeBranches", True),
     (runTargetTransforms AugmentPreRW, "runTargetTransforms", True),
     (addPrologueEpilogue, "addPrologueEpilogue", implementFrames),
     (cleanAttributes, "cleanAttributes", True),
     (renameTemps, "renameTemps", True),
     (renameMOperands, "renameMOperands", True),
     (renameOperations, "renameOperations", True),
     (addReadWrites, "addReadWrites", True),
     (liftMemInfo, "liftMemInfo", True),
     (runTargetTransforms AugmentPostRW, "runTargetTransforms", True),
     (cleanPragmas augmentPragmaTools, "cleanPragmas", True),
     (addPragmas augmentPragmas, "addPragmas", True)]

augmentPragmas =
    [("lint",
      "--nocostoverflow=false")]

augmentPragmaTools = map fst augmentPragmas
