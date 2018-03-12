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
module Unison.Tools.Lint (run, invokeLint) where

import System.Exit
import Control.Monad

import Unison.Driver
import Unison.Parser
import qualified Unison.Test.Invariants as I
import Unison.Tools.UniArgs
import Unison.Base
import Unison.Util

import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

run rawArgs uni target =
  do let f = parse target uni
     success <- runLint rawArgs f target
     if success then exitSuccess else exitFailure

runLint rawArgs f target =
  do let ps   = pragmas "lint" (fComments f)
         argv = processValue uniArgs (rawArgs ++ ps)
     args <- cmdArgsApply argv
     let result = map (test f target)
                  [(I.allTemporariesDefined, allTemporariesDefined args),
                   (I.singleDefinitions, singleDefinitions args),
                   (I.allTemporariesUsed, allTemporariesUsed args),
                   (I.allRegistersDefined, allRegistersDefined args),
                   (I.allRegClassesDefined, allRegClassesDefined args),
                   (I.noEmptyRegClass, noEmptyRegClass args),
                   (I.consistentOperandInfo, consistentOperandInfo args),
                   (I.consistentOperands, consistentOperands args),
                   (I.consistentPreAssignments, consistentPreAssignments args),
                   (I.noRedefinitions, noRedefinitions args),
                   (I.noEdgeInterferences, noEdgeInterferences args),
                   (I.noMustConflicts, noMustConflicts args),
                   (I.noCongruentCopy, noCongruentCopy args),
                   (I.noIsolatedGlobals, noIsolatedGlobals args),
                   (I.uniqueOperationIds, uniqueOperationIds args),
                   (I.uniqueOperandIds, uniqueOperandIds args),
                   (I.singleEntryBlock, singleEntryBlock args),
                   (I.allEntryOpsPreAssigned, allEntryOpsPreAssigned args),
                   (I.allExitOpsPreAssigned, allExitOpsPreAssigned args),
                   (I.noComponentConflicts, noComponentConflicts args),
                   (I.noCostOverflow, noCostOverflow args),
                   (I.noAmbiguousPhis, noAmbiguousPhis args),
                   (I.allResourcesDefined, allResourcesDefined args),
                   (I.allRegClassesReal, allRegClassesReal args),
                   (I.noReservedRegRedef, noReservedRegRedef args),
                   (I.noEmptyBlock, noEmptyBlock args)]
     emitOutput (outFile args) (showProblems result)
     return $ null $ concat result

invokeLint f target =
  do success <- runLint ["lint", "--target=foo", "bar"] f target
     when (not success) exitFailure

test f target (invariantTest, p) = if p then invariantTest f target else []

showProblems = unlines . concat
