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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, NoMonomorphismRestriction #-}
module Unison.Tools.Linearize (run) where

import System.FilePath
import Control.Monad

import Unison.Driver
import Unison.Parser
import Unison.Tools.Lint (invokeLint)

import Unison.Transformations.RenameTemps
import Unison.Transformations.SortGlobalTemps
import Unison.Transformations.RenameOperations
import Unison.Transformations.PropagatePhiCongruences
import Unison.Transformations.CleanPragmas
import Unison.Transformations.AddPragmas

import Unison.Tools.Linearize.SinkLiveOuts
import Unison.Tools.Linearize.SourceLiveIns
import Unison.Tools.Linearize.RemovePhis
import Unison.Tools.Linearize.NormalizeCongruences
import Unison.Tools.Linearize.AddReflexiveCongruences

run (uniFile, debug, intermediate, lint, lintPragma, lssaUniFile) uni target =
    let f = parse target uni
        (lssaF, partialLssaFs) =
            applyTransformations
            (linearizerTransformations lintPragma)
            target f
        baseName = takeBaseName uniFile
    in do when debug $
               putStr (toPlainText partialLssaFs)
          when intermediate $
               mapM_ (writeIntermediateFile "lssa.uni" baseName) partialLssaFs
          emitOutput lssaUniFile (show lssaF)
          when lint $
               invokeLint lssaF target

linearizerTransformations lintPragma =
    [(propagatePhiCongruences, "propagatePhiCongruences", True),
     (removePhis, "removePhis", True),
     (sinkLiveOuts, "sinkLiveOuts", True),
     (sourceLiveIns, "sourceLiveIns", True),
     (renameTemps, "renameTemps", True),
     (sortGlobalTemps, "sortGlobalTemps", True),
     (normalizeCongruences, "normalizeCongruences", True),
     (addReflexiveCongruences, "addReflexiveCongruences", True),
     (renameOperations, "renameOperations", True),
     (cleanPragmas linearizePragmaTools, "cleanPragmas", True),
     (addPragmas linearizePragmas, "addPragmas", lintPragma)]

linearizePragmas =
    [("lint",
      "--noedgeinterferences=false " ++
      "--nomustconflicts=false " ++
      "--nocongruentcopy=false " ++
      "--nocostoverflow=false " ++
      "--nocomponentconflicts=false")]

linearizePragmaTools = map fst linearizePragmas
