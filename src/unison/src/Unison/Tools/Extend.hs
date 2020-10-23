{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, NoMonomorphismRestriction #-}
module Unison.Tools.Extend (run) where

import System.FilePath
import Control.Monad

import Unison.Driver
import Unison.Parser
import Unison.Tools.Lint (invokeLint)

import Unison.Transformations.PostponeBranches
import Unison.Transformations.RenameTemps
import Unison.Transformations.RenameOperations
import Unison.Transformations.CleanPragmas
import Unison.Transformations.AddPragmas

import Unison.Tools.Extend.ExtendWithCopies
import Unison.Tools.Extend.SortCopies
import Unison.Tools.Extend.GroupCalls

run (uniFile, debug, intermediate, lint, lintPragma, extUniFile) uni target =
    let f = parse target uni
        (extF, partialExtFs) =
            applyTransformations
            (extenderTransformations lintPragma)
            target f
        baseName = takeBaseName uniFile
    in do when debug $
               putStr (toPlainText partialExtFs)
          when intermediate $
               mapM_ (writeIntermediateFile "ext.uni" baseName) partialExtFs
          emitOutput extUniFile (show extF)
          when lint $
               invokeLint extF target

extenderTransformations lintPragma =
    [(extendWithCopies, "extendWithCopies", True),
     (sortCopies, "sortCopies", True),
     (postponeBranches, "postponeBranches", True),
     (groupCalls, "groupCalls", True),
     (renameTemps, "renameTemps", True),
     (renameOperations, "renameOperations", True),
     (cleanPragmas extendPragmaTools, "cleanPragmas", True),
     (addPragmas extendPragmas, "addPragmas", lintPragma)]

extendPragmas =
    [("lint",
      "--nocostoverflow=false")]

extendPragmaTools = map fst extendPragmas
