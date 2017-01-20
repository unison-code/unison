#!/usr/bin/env runhaskell

{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Runs 'gecode-solver' and 'minizinc-solver' (with 'chuffed') in parallel,
returning the result of the fastest solver.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Console.CmdArgs
import System.Process
import System.Process.Internals
import System.Directory
import Data.List.Split

data Portfolio =
  Portfolio {inFile  :: FilePath,
             outFile :: FilePath,
             verbose :: Bool}
  deriving (Data, Typeable, Show)

portfolioArgs = cmdArgsMode $ Portfolio
    {
     inFile  = "" &= argPos 1 &= typFile,
     outFile = "" &= name "o" &= help "Output file name" &= typFile,
     verbose = False &= name "v" &= help "Run solvers in verbose mode"
    }

gecodeFile outJsonFile = outJsonFile ++ ".gecode"
chuffedFile outJsonFile = outJsonFile ++ ".chuffed"

runGecode v extJson outJsonFile =
  do callProcess "gecode-solver"
       ["-o", outJsonFile] ++ ["--verbose" | v] ++ [extJson]
     return outJsonFile

runChuffed extJson outJsonFile =
  do callProcess "minizinc-solver"
       ["--topdown", "--chuffed", "--free", "-o", outJsonFile, extJson]
     return outJsonFile

main =
    do Portfolio{..} <- cmdArgsRun portfolioArgs
       let gecodeOutFile  = outFile ++ ".gecode"
           chuffedOutFile = outFile ++ ".chuffed"
       result <- race
                 (runGecode verbose inFile gecodeOutFile)
                 (runChuffed inFile chuffedOutFile)
       -- FIXME: this is too brutal (and platform-specific), find out instead
       -- why the process spawned by 'minizinc-solver' are not killed
       createProcess
         (proc "killall" ["sicstus", "mzn2fzn", "fzn-chuffed"])
         {std_out = CreatePipe, std_err = CreatePipe}
       let finalOutFile = case result of
                           Left  outFile1 -> outFile1
                           Right outFile2 -> outFile2
       renameFile finalOutFile outFile
       removeIfExists gecodeOutFile
       removeIfExists chuffedOutFile
       return ()

removeIfExists file =
  do fileExists <- doesFileExist file
     when fileExists (removeFile file)
