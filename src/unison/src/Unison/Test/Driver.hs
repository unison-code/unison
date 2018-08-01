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
{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, CPP #-}
module Unison.Test.Driver where

import Data.Maybe
import System.Exit
import Test.HUnit
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Directory
import System.IO
import Control.Monad
import System.FilePath hiding (addExtension)
import Data.List
import Data.Yaml
import Data.List.Split
#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>),(<*>))
#endif
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Common.Util
import qualified MachineIR as MIR
import Unison
import Unison.Target.API
import Unison.Tools.Run as Run
import Unison.Tools.UniArgs hiding (verbose, presolver, solver)
import Unison.Driver
import qualified Unison.Tools.Analyze as Analyze

data TestArgs = TestArgs {directory :: FilePath, presolver :: Maybe FilePath,
                          solver :: Maybe FilePath, showProgress :: Bool,
                          verbose :: Bool, update :: Bool}
              deriving (Data, Typeable, Show)

testArgs = cmdArgsMode $ TestArgs
    {
      directory    = "" &= name "d" &= help "Test directory" &= typFile,
      presolver    = Nothing &= help "Path to Unison's presolver binary" &= typFile,
      solver       = Nothing &= help "Path to Unison's solver binary" &= typFile,
      showProgress = False &= name "p" &= help "Show test progress",
      verbose      = False &= name "v" &= help "Run tests in verbose mode",
      update       = False &= help "Update test expectations with the latest results from Unison"
    }

mainWithTargets unisonTargets =
  do testArgs <- cmdArgsRun testArgs
     mirFiles <- mirFileNames (directory testArgs)
     let tests = TestList $ map (systemTest unisonTargets testArgs) mirFiles
     count <- runTestTT tests
     exitTest count

exitTest Counts {errors = 0, failures = 0} = exitSuccess
exitTest _ = exitFailure

systemTest unisonTargets testArgs mirFile =
  TestList
  [TestLabel mirFile (TestCase (runUnison unisonTargets testArgs mirFile))]

runUnison unisonTargets testArgs mirFile =
  do let argv = processValue uniArgs ["run", "--target=foo", "bar"]
     args <- cmdArgsApply argv
     mir <- strictReadFile mirFile
     let sp = showProgress testArgs
         verb = verbose testArgs
         upd = update testArgs
         asmMirFile = addExtension "asm.mir" (take (length mirFile - 4) mirFile)
         properties = parseTestProperties unisonTargets mirFile mir
     when (verb || sp) $ hPutStrLn stderr ""
     when sp $ hPutStrLn stderr ("Running test " ++ mirFile ++ "...")
     case pickTarget (testTarget properties) unisonTargets of
      (Any target) -> do
        [prefix]
               <- Run.run
                  (estimateFreq args,
                   simplifyControlFlow args,
                   noCC args,
                   noReserved args,
                   maxBlockSize args,
                   implementFrames args,
                   (testFunction properties),
                   Just (intercalate "," (testGoal properties)),
                   noCross args,
                   oldModel args,
                   expandCopies args,
                   rematType args,
                   Just asmMirFile,
                   scaleFreq args,
                   applyBaseFile args,
                   tightPressureBound args,
                   fromMaybe (strictlyBetter args) (testStrictlyBetter properties),
                   unsatisfiable args,
                   removeReds args,
                   keepNops args,
                   [],
                   ["-local-limit=4000", "-total-threads=1", "-portfolio-threads=1"],
                   fromMaybe (mirVersion args) (testMirVersion properties),
                   mirFile,
                   False,
                   verb,
                   intermediate args,
                   True,
                   Nothing,
                   True,
                   False,
                   presolver testArgs,
                   solver testArgs,
                   Nothing,
                   False)
                  (target, targetOption args)
        properties1 <- assertOutJson upd properties prefix
        let unisonMirFile = addExtension "unison.mir" prefix
        properties2 <- assertCost (mirVersion args) upd
                       (target, targetOption args)
                       properties1 unisonMirFile
        when upd $ updateProperties properties2 mirFile mir
        return ()

addExtension ext prefix = prefix ++ "." ++ ext

mirFileNames dir = do
  names <- getDirectoryContents dir
  paths <- forM (filter (`notElem` [".", ".."]) names) $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then mirFileNames path
      else return [path | ".mir" `isSuffixOf` path,
                          not (".asm.mir" `isSuffixOf` path)]
  return (concat paths)

parseTestProperties unisonTargets mirFile mir =
  let docs = split onDocumentEnd mir
  in case decodeEither $ B8.pack (last docs) of
      Right properties -> properties :: TestProperties
      (Left _) -> mkTestProperties (guessTarget unisonTargets mirFile)
                  (guessGoal mirFile)
                  Nothing Nothing Nothing Nothing Nothing Nothing

guessTarget unisonTargets mirFile =
  case find (\t -> t `isInfixOf` mirFile) (map fst unisonTargets) of
   Just target -> target
   Nothing -> error ("File " ++ show mirFile ++ " does not specify the target")

guessGoal mirFile =
  case find (\g -> g `isInfixOf` mirFile) ["speed", "size"] of
   Just goal -> [goal]
   Nothing -> error ("File " ++ show mirFile ++ " does not specify the goal")

updateProperties properties mirFile mir =
  do let docs = split onDocumentEnd mir
         mir' = dummyIR ++ findMirDoc properties docs ++
                "---\n" ++ encodeYaml properties ++ "...\n"
     writeFile mirFile mir'

dummyIR = "--- |\n" ++ "  ; ModuleID = 'dummy.ll'\n" ++ "...\n"

findMirDoc properties docs =
  case filter isMachineFunctionDoc docs of
   [mf] -> mf
   mfs  -> case testFunction properties of
            Just function ->
              fromJust $ find (\mf -> machineFunctionName mf == function) mfs
            Nothing -> error "cannot disambiguate function to be updated"

isMachineFunctionDoc doc = namePrefix `isPrefixOf` doc

machineFunctionName doc =
  takeWhile ((/=) '\n') $ dropWhile ((==) ' ') $ drop (length namePrefix) doc

namePrefix = "---\nname:"

mkTestProperties = TestProperties

data TestProperties = TestProperties {
  testTarget :: String,
  testGoal :: [String],
  testFunction :: Maybe String,
  testStrictlyBetter :: Maybe Bool,
  testMirVersion :: Maybe MIR.MachineIRVersion,
  testExpectedHasSolution :: Maybe Bool,
  testExpectedProven :: Maybe Bool,
  testExpectedCost :: Maybe [Integer]
  } deriving Show

instance FromJSON TestProperties where
  parseJSON (Object v) =
    TestProperties <$>
    (v .:  "unison-test-target") <*>
    (v .:  "unison-test-goal") <*>
    (v .:? "unison-test-function") <*>
    (v .:? "unison-test-strictly-better") <*>
    (v .:? "unison-test-mir-version") <*>
    (v .:? "unison-test-expected-has-solution") <*>
    (v .:? "unison-test-expected-proven") <*>
    (v .:? "unison-test-expected-cost")
  parseJSON _ = error "Can't parse TestProperties from YAML"

instance ToJSON TestProperties where
  toJSON (TestProperties target goal fun sb mv expHasSolution expProven expCost) =
    object ["unison-test-target" .= target,
            "unison-test-goal" .= goal,
            "unison-test-function" .= fun,
            "unison-test-strictly-better" .= sb,
            "unison-test-mir-version" .= mv,
            "unison-test-expected-has-solution" .= expHasSolution,
            "unison-test-expected-proven" .= expProven,
            "unison-test-expected-cost" .= expCost]

assertOutJson update properties prefix =
  do let outJsonFile = addExtension "out.json" prefix
     outJson <- strictReadFile outJsonFile
     let sol = parseSolution outJson
         expHasSolution = solFromJson sol "has_solution" :: Bool
         properties1 = if update then properties {testExpectedHasSolution =
                                                     Just expHasSolution}
                       else properties
         expProven = solFromJson sol "proven" :: Bool
         properties2 = if update then properties1 {testExpectedProven =
                                                      Just expProven}
                       else properties1
         hasSolution = testExpectedHasSolution properties2
         proven = testExpectedProven properties2
     when (isJust hasSolution) $
       assertEqual
       "* unexpected 'has_solution' value"
       (fromJust hasSolution)
       expHasSolution
     when (isJust proven) $
       assertEqual
       "* unexpected 'proven' value"
       (fromJust proven)
       expProven
     return properties2

assertCost mirVersion update target properties unisonMirFile =
    do unisonMir <- strictReadFile unisonMirFile
       let gls = map lowLevelGoal (testGoal properties)
           (expCosts, _) =
             Analyze.analyze (False, True, mirVersion, False, False)
             1.0 gls unisonMir target
           properties1 = if update then properties {testExpectedCost =
                                                       Just expCosts}
                         else properties
           costs = testExpectedCost properties1
       when (isJust costs) $
         assertEqual
         "* unexpected 'cost' value"
         (fromJust costs)
         expCosts
       return properties1

lowLevelGoal "speed" = DynamicGoal Cycles
lowLevelGoal "size"  = StaticGoal (ResourceUsage (read "BundleWidth"))

parseSolution json =
  case JSON.decode (BSL.pack json) of
   Nothing -> error ("error parsing JSON input")
   Just (Object s) -> s

solFromJson sol key =
    case JSON.fromJSON (sol HM.! key) of
      JSON.Error e -> error ("error converting JSON input:\n" ++ show e)
      JSON.Success s -> s

instance ToJSON MIR.MachineIRVersion where
  toJSON v = toJSON (show v)

instance FromJSON MIR.MachineIRVersion where
  parseJSON = JSON.withText "String" (\v -> return (read (T.unpack v)))
