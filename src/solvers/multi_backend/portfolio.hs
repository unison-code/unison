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

{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

import Data.Aeson
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import System.Console.CmdArgs
import System.Process
import System.Process.Internals
import System.Directory
import Data.List.Split

data Portfolio =
  Portfolio {inFile  :: FilePath,
             outFile :: FilePath,
             verbose :: Bool,
             gecodeFlags :: String,
             chuffedFlags :: String}
  deriving (Data, Typeable, Show)

data Solver = Gecode | Chuffed deriving Eq

portfolioArgs = cmdArgsMode $ Portfolio
    {
     inFile  = "" &= argPos 1 &= typFile,
     outFile = "" &= name "o" &= help "Output file name" &= typFile,
     verbose = False &= name "v" &= help "Run solvers in verbose mode",
     gecodeFlags = "" &= help "Flags passed to the Gecode solver",
     chuffedFlags = "" &= help "Flags passed to the Chuffed solver"
    }

gecodeFile :: FilePath -> String
gecodeFile outJsonFile = outJsonFile ++ ".gecode"
chuffedFile :: FilePath -> String
chuffedFile outJsonFile = outJsonFile ++ ".chuffed"

runGecode flags v extJson outJsonFile =
  do callProcess "gecode-solver"
       (["-o", outJsonFile] ++ ["--verbose" | v] ++ (splitFlags flags) ++
        [extJson])
     return outJsonFile

runChuffed flags extJson outJsonFile =
  do callProcess "minizinc-solver"
       (["--topdown", "--chuffed", "--free", "-o", outJsonFile] ++
        (splitFlags flags) ++
        [extJson])
     out <- readFile outJsonFile
     -- if chuffed terminated without a proof, that means there was an error
     when (not (proven out)) $ forever $ threadDelay 10000
     return outJsonFile

splitFlags :: String -> [String]
splitFlags flags =
  let flags' = map replaceFlagChar flags
  in [flag | flag <- splitOn " " flags', not (null flag)]

proven json =
  let sol    = parseJson json
      proven = sol HM.! "proven"
  in (solutionFromJson proven) :: Bool

parseJson json =
  case decode (BSL.pack json) of
   Nothing -> error ("error parsing JSON output")
   Just (Object s) -> s

solutionFromJson object =
  case fromJSON object of
   Error e -> error ("error converting JSON input:\n" ++ show e)
   Success s -> s

main =
    do Portfolio{..} <- cmdArgsRun portfolioArgs
       let gecodeOutFile  = outFile ++ ".gecode"
           chuffedOutFile = outFile ++ ".chuffed"
           chuffedLastOutFile = outFile ++ ".chuffed.last"
       result <- race
                 (runGecode gecodeFlags verbose inFile gecodeOutFile)
                 (runChuffed chuffedFlags inFile chuffedOutFile)
       -- FIXME: this is too brutal (and platform-specific), find out instead
       -- why the process spawned by 'minizinc-solver' are not killed
       createProcess
         (proc "killall" ["sicstus", "mzn2fzn", "fzn-chuffed"])
         {std_out = CreatePipe, std_err = CreatePipe}
       let winner = case result of
                      Left  _ -> Gecode
                      Right _ -> Chuffed
       finalOutFile <- if winner == Chuffed then return chuffedOutFile
                       else
                         do gecodeOut <- readFile gecodeOutFile
                            if proven gecodeOut then return gecodeOutFile
                            else
                            -- gecode-solver timed out, the last solution
                            -- provided by Chuffed might be actually better
                                 do chuffedLastOut <- readIfExists
                                                   chuffedLastOutFile
                                    if chuffedLastOut `betterThan` gecodeOut
                                       then return chuffedLastOutFile
                                       else return gecodeOutFile
       renameFile finalOutFile outFile
       removeIfExists gecodeOutFile
       removeIfExists chuffedOutFile
       removeIfExists chuffedLastOutFile
       return ()

replaceFlagChar '=' = ' '
replaceFlagChar ';' = ' '
replaceFlagChar c = c

removeIfExists file =
  do fileExists <- doesFileExist file
     when fileExists (removeFile file)

readIfExists file =
  do fileExists <- doesFileExist file
     if fileExists then readFile file else return ""

betterThan out1 out2 = cost out1 < cost out2

cost "" = maxInt
cost out =
  let sol  = parseJson out
      cost = sol HM.! "cost"
      c    = (solutionFromJson cost) :: Integer
  in if c == -1 then maxInt else c

maxInt = toInteger (maxBound - 1 :: Int32)
