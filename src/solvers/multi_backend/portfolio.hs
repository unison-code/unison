#!/usr/bin/env runhaskell

{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Runs 'gecode-solver' and 'minizinc-solver' (with 'chuffed') in parallel,
returning the result of the fastest solver.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Int
import Data.Ord
import Data.List
import qualified Data.Text.Lazy.Builder as DTB
import Data.Scientific
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import System.Console.CmdArgs
import System.Process
import System.Process.Internals
import System.Directory
import System.IO
import System.Environment
import System.Timeout
import Data.Time
import Data.List.Split

data Portfolio =
  Portfolio {inFile  :: FilePath,
             outFile :: FilePath,
             verbose :: Bool,
             gecodeFlags :: String,
             chuffedFlags :: String,
             timeOut :: Maybe Integer,
             memLimit :: Bool,
             lowerBoundFile :: FilePath}
  deriving (Data, Typeable, Show)

data Solver = Gecode | Chuffed | NoSolver deriving Eq

portfolioArgs = cmdArgsMode $ Portfolio
    {
     inFile  = "" &= argPos 1 &= typFile,
     outFile = "" &= name "o" &= help "Output file name" &= typFile,
     verbose = False &= name "v" &= help "Run solvers in verbose mode",
     gecodeFlags = "" &= help "Flags passed to the Gecode solver",
     chuffedFlags = "" &= help "Flags passed to the Chuffed solver",
     timeOut = Nothing &= help "Timeout for both solvers (in seconds)",
     memLimit = True &= help "Limit the use of memory by Chuffed to 16 GB",
     lowerBoundFile = "" &= name "l" &= help "Lower bound file" &= typFile
    }

gecodeFile :: FilePath -> String
gecodeFile outJsonFile = outJsonFile ++ ".gecode"
chuffedFile :: FilePath -> String
chuffedFile outJsonFile = outJsonFile ++ ".chuffed"

runGecode flags to v extJson lowerBoundFile outJsonFile =
  do tryUntilSuccess $ callProcess "gecode-solver"
       (["-o", outJsonFile] ++
        (if null lowerBoundFile then [] else ["-l", lowerBoundFile]) ++
        ["-verbose" | v] ++ (splitFlags flags) ++
        (gecodeTimeoutFlags to) ++
        [extJson])
     return outJsonFile

gecodeTimeoutFlags to
  | to >= 0 = ["-complete", "-all-solutions"]
  | otherwise = []

tryIO :: IO a ->  IO (Either IOException a)
tryIO =  try

tryUntilSuccess a =
  do result <- tryIO $ a
     case result of
      Left ex ->
        do putStrLn $ show ex ++ ", trying again..."
           tryUntilSuccess a
      Right () -> return ()

runChuffed flags to memLimit extJson lowerBoundFile outJsonFile =
  do -- call 'minizinc-solver' but only for the setup (we would like to use the
     -- entire script but for some reason then we cannot kill the underlying
     -- processes when MiniZinc looses the race)
     callProcess "minizinc-solver"
       (["--topdown", "--chuffed", "--no-diffn", "--free", "--rnd",
         "--setuponly"] ++
        (if null lowerBoundFile then [] else ["-l", lowerBoundFile]) ++
        (splitFlags flags) ++
        [extJson])
     -- now call the underlying 'minizinc' process that is killable (unlike
     -- what is executed from 'minizinc-solver')
     let mznChuffed = if memLimit then "mzn-crippled-chuffed" else "mzn-chuffed"
         pre = (take (length extJson - 9) extJson)
         mzn = pre ++ ".mzn"
         dzn = pre ++ ".dzn"
         out = pre ++ ".out"
     setEnv "FLATZINC_CMD" "fzn-chuffed"
     tryUntilSuccess $ callProcess mznChuffed
       (concatMap fznFlag (["--verbosity", "3",
                            "-f",
                            "--rnd-seed", "123456"] ++
                           chuffedTimeoutFlags to) ++
        ["-a", "-s",
         "-D", "good_cumulative=true",
         "-D", "good_diffn=false"] ++
        [mzn, dzn, "-o", out])
     -- finally, invoke 'outfilter' to format the output
     inf  <- openFile out ReadMode
     outf <- openFile outJsonFile WriteMode
     (_, _, _, h) <- createProcess
                     (proc "outfilter.pl"
                           [outJsonFile ++ ".last",
                            if null lowerBoundFile then "-" else lowerBoundFile])
                       {std_in = UseHandle inf, std_out = UseHandle outf}
     waitForProcess h
     hClose inf
     hClose outf
     return outJsonFile

fznFlag opt = ["--fzn-flag", opt]

chuffedTimeoutFlags to
  -- just to be sure that fzn-chuffed dies after the timeout (it does not
  -- always honor the kill signal)
  | to >= 0 = ["--time-out", show (to `div` timeoutFactor)]
  | otherwise = []

splitFlags :: String -> [String]
splitFlags flags =
  let flags' = map replaceFlagChar flags
  in [flag | flag <- splitOn " " flags', not (null flag)]

proven json =
  let sol    = parseJson json
      proven = lookupOrFail "sol" sol "proven"
  in (fromJson proven) :: Bool

parseJson json =
  case decode (BSL.pack json) of
   Nothing -> error ("error parsing JSON output")
   Just (Object s) -> s

validJson json =
  case decode (BSL.pack json) :: Maybe Value of
   (Just _) -> True
   Nothing  -> False

fromJson object =
  case fromJSON object of
   Error e -> error ("error converting JSON input:\n" ++ show e)
   Success s -> s

strictReadFile f =
  do h <- openFile f ReadMode
     c <- strictHGetContents h
     return c

strictHGetContents h =
  do c <- hGetContents h
     length c `seq` return c

timeoutFactor = 1000000

main =
    do Portfolio{..} <- cmdArgsRun portfolioArgs
       let baseOutFile    = outFile ++ ".base"
           gecodeOutFile  = outFile ++ ".gecode"
           chuffedOutFile = outFile ++ ".chuffed"
           chuffedLastOutFile = outFile ++ ".chuffed.last"
           gecodeLowerBoundFile = lowerBoundFile ++ ".gecode"
           chuffedLowerBoundFile = lowerBoundFile ++ ".chuffed"
           to = case timeOut of
                 Just s  -> if s * timeoutFactor > maxInt
                            then error ("exceeded maximum timeout")
                            else s * timeoutFactor
                 Nothing -> -1
       start <- getCurrentTime
       result <- timeout (fromInteger to)
                 (race
                  (runGecode gecodeFlags to verbose inFile
                   gecodeLowerBoundFile gecodeOutFile)
                  (runChuffed chuffedFlags to memLimit inFile
                   chuffedLowerBoundFile chuffedOutFile))
       end <- getCurrentTime
       let winner = case result of
                     (Just (Left  _)) -> Gecode
                     (Just (Right _)) -> Chuffed
                     Nothing          -> NoSolver
           solverTime = (round $ fromRational $
                         toRational (diffUTCTime end start) * 1000) :: Integer
       finalOutFile <- if to >= 0
                       then pickBest winner baseOutFile gecodeOutFile
                            (chuffedOutFile, chuffedLastOutFile)
                       else pickNoTimeOutBest winner gecodeOutFile
                            (chuffedOutFile, chuffedLastOutFile)
       renameFile finalOutFile outFile
       extJsonStr <- readIfExists inFile
       let input   = parseJson extJsonStr
           factor  = freqFactor input
           [dyn]   = optimizeDynamic input -- portfolio-solver assumes one goal
           preTime = presolverTime input
       writeLowerBoundFile (dyn, factor) lowerBoundFile
         [gecodeLowerBoundFile, chuffedLowerBoundFile] outFile
       updateTimes preTime solverTime outFile
       removeIfExists gecodeOutFile
       removeIfExists chuffedOutFile
       removeIfExists chuffedLastOutFile
       return ()

pickBest winner baseOutFile gecodeOutFile (chuffedOutFile, chuffedLastOutFile)
  | winner == Gecode  = return gecodeOutFile
  | winner == Chuffed = return chuffedOutFile
  | winner == NoSolver = -- see which one produced the best non-proven solution
    do gecodeOut  <- readIfExists gecodeOutFile
       chuffedOut <- readIfExists chuffedLastOutFile
       let (best, bestOut) = minimumBy (comparing solutionCost)
                             [(gecodeOutFile, gecodeOut),
                              (chuffedLastOutFile, chuffedOut)]
       (best', bestOut') <- maybeBase baseOutFile (best, bestOut)
       return best'

maybeBase baseOutFile (best, bestOut) =
  if solutionCost (best, bestOut) < maxInt then return (best, bestOut) else
    do writeFile baseOutFile baseOut
       return (baseOutFile, baseOut)

baseOut = toJSONString $ M.fromList baseSolution
baseSolution =
  [("solver", toJSON ("no-solver" :: String)),
   ("cost", toJSON (-1 :: Integer)),
   ("has_solution", toJSON False),
   ("proven" :: String, toJSON False)]

pickNoTimeOutBest winner gecodeOutFile (chuffedOutFile, chuffedLastOutFile) =
  if winner == Chuffed then return chuffedOutFile
  else
    do gecodeOut <- strictReadFile gecodeOutFile
       if proven gecodeOut then return gecodeOutFile
         else
         -- gecode-solver timed out, the last solution
         -- provided by Chuffed might be actually better
         do chuffedLastOut <- readIfExists
                              chuffedLastOutFile
            if chuffedLastOut `betterThan` gecodeOut
              then return chuffedLastOutFile
              else return gecodeOutFile

replaceFlagChar '=' = ' '
replaceFlagChar ';' = ' '
replaceFlagChar c = c

removeIfExists file =
  do fileExists <- doesFileExist file
     when fileExists (removeFile file)

readIfExists file =
  do fileExists <- doesFileExist file
     if fileExists then strictReadFile file else return ""

betterThan out1 out2 = cost out1 < cost out2

solutionCost (_, "")  = maxInt
solutionCost (_, out) = cost out

cost "" = maxInt
cost out
  | not (validJson out) = maxInt
cost out =
  let sol  = parseJson out
      cost = lookupOrFail "sol" sol "cost"
      -- Assumes a single goal as the minizinc solver does not support more
      [c]  = (fromJson cost) :: [Integer]
  in if c == -1 then maxInt else c

maxInt = toInteger (maxBound - 1 :: Int32)

lowerBound lowerBoundJson =
  let json  = parseJson lowerBoundJson
      lbj   = lookupOrFail "json" json "lower_bound"
      [lb]  = (fromJson lbj) :: [Integer]
  in lb

freqFactor input =
  let factor = lookupOrFail "input" input "freq_scale"
  in fromJson factor :: Double

optimizeDynamic input =
  let od = lookupOrFail "input" input "optimize_dynamic"
  in fromJson od :: [Bool]

writeLowerBoundFile _ "" _ _ = return ()
-- Writes final lower bound to 'outLowerBoundFile' according to the
-- solution properties and the lower bounds provided by the solvers
writeLowerBoundFile (dyn, factor) outLowerBoundFile inLowerBoundFiles outFile =
  do bestOut <- readIfExists outFile
     jsonLBs <- mapM readIfExists inLowerBoundFiles
     let lbs = [lowerBound lb | lb <- jsonLBs, validJson lb]
         bestLB = if proven bestOut || null lbs then baseLowerBound
                  else
                    let lb  = maximum lbs
                        lb1 = if dyn then
                                floor ((fromInteger lb) / factor)
                              else lb
                    in formatLB lb1
     writeFile outLowerBoundFile bestLB

baseLowerBound = formatLB (-1)
formatLB lb =
  toJSONString $ M.fromList [("lower_bound" :: String, toJSON [lb :: Integer])]

updateTimes preTime solverTime outFile =
  do outJsonStr <- readIfExists outFile
     let outJson  = parseJson outJsonStr
         -- override existing fields possibly emitted by the underlying solvers
         outJson' = HM.union
                    (HM.fromList [("presolver_time", toJSON preTime),
                                  ("solver_time", toJSON solverTime)])
                    outJson
     writeFile outFile (toJSONMap outJson')
     return ()

presolverTime input =
  let preTime = lookupOrFail "input" input "presolver_time"
  in fromJson preTime :: Integer

toJSONMap = BSL.unpack . encodePretty' jsonConfig
toJSONString = BSL.unpack . encodePretty' jsonConfig . toJSON
jsonConfig = defConfig {confNumFormat = Custom showInteger}
showInteger i =
  case floatingOrInteger i of
   Right i' -> DTB.fromString (show (toInteger i'))
   Left r -> error ("expecting integer but got " ++ show r)

lookupOrFail name map key =
  case HM.lookup  key map of
   Just val -> val
   Nothing -> error $ "key " ++ show key ++ " is not present in map " ++
                      show name ++ "\n" ++ "map contents: " ++ show map
