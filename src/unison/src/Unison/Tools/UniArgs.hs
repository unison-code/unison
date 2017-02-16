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
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Unison.Tools.UniArgs (Uni(..), uniArgs) where

import System.Console.CmdArgs

data Uni =
    Import    {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, intermediate :: Bool,
               lint :: Bool, lintPragma :: Bool, estimateFreq :: Bool,
               simplifyControlFlow :: Bool, implementFrames :: Bool,
               noCC :: Bool, noReserved :: Bool, maxBlockSize :: Maybe Integer,
               function :: Maybe String, goal :: Maybe String} |
    Linearize {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, intermediate :: Bool,
               lint :: Bool, lintPragma :: Bool} |
    Extend    {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, intermediate :: Bool,
               lint :: Bool, lintPragma :: Bool} |
    Augment   {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, intermediate :: Bool,
               lint :: Bool, lintPragma :: Bool, implementFrames :: Bool,
               noCross :: Bool, oldModel :: Bool, expandCopies :: Bool,
               rematerialize :: Bool} |
    Model     {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, baseFile :: Maybe FilePath,
               scaleFreq :: Bool, oldModel :: Bool, applyBaseFile :: Bool,
               tightPressureBound :: Bool, strictlyBetter :: Bool,
               unsatisfiable :: Bool} |
    Export    {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, removeReds :: Bool,
               keepNops :: Bool, baseFile :: Maybe FilePath,
               tightPressureBound :: Bool, solFile :: Maybe FilePath} |
    Analyze   {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, intermediate :: Bool,
               goals :: String, estimateFreq :: Bool,
               simulateStalls :: Bool, modelCost :: Bool} |
    Normalize {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, estimateFreq :: Bool,
               simplifyControlFlow :: Bool} |
    Lint      {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, allTemporariesDefined :: Bool,
               singleDefinitions :: Bool, allTemporariesUsed :: Bool,
               allRegistersDefined :: Bool, noReadWriteOverlaps :: Bool,
               allRegClassesDefined :: Bool, noEmptyRegClass :: Bool,
               consistentOperandInfo :: Bool, consistentOperands :: Bool,
               consistentPreAssignments :: Bool, noRedefinitions :: Bool,
               noEdgeInterferences :: Bool, noMustConflicts :: Bool,
               noCongruentCopy :: Bool, noIsolatedGlobals :: Bool,
               uniqueOperationIds :: Bool, uniqueOperandIds :: Bool,
               singleEntryBlock :: Bool, allEntryOpsPreAssigned :: Bool,
               allExitOpsPreAssigned :: Bool, noComponentConflicts :: Bool,
               noCostOverflow :: Bool, noAmbiguousPhis :: Bool,
               allResourcesDefined :: Bool, allRegClassesReal :: Bool,
               noReservedRegRedef :: Bool, noEmptyBlock :: Bool} |
    Count     {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, singleRow :: Bool} |
    Legalize  {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath} |
    Plot      {targetName :: String, inFile :: FilePath, targetOption :: [String],
               genBcfg :: Bool, genIcfg :: Bool, genBdt :: Bool, genDg :: Bool,
               genCg :: Bool, genSg :: Bool, genOg :: Bool, genPg :: Bool,
               genPpg :: Bool, graphBlock :: Maybe Int, simpleGraph :: Bool} |
    Run       {targetName :: String, inFile :: FilePath, targetOption :: [String],
               outFile :: Maybe FilePath, debug :: Bool, verbose :: Bool,
               intermediate :: Bool, lint :: Bool, estimateFreq :: Bool,
               simplifyControlFlow :: Bool, implementFrames :: Bool, noCC :: Bool,
               noReserved :: Bool, maxBlockSize :: Maybe Integer, function :: Maybe String,
               goal :: Maybe String, noCross :: Bool, oldModel :: Bool,
               expandCopies :: Bool, rematerialize :: Bool,
               baseFile :: Maybe FilePath, scaleFreq :: Bool,
               applyBaseFile :: Bool, tightPressureBound :: Bool,
               strictlyBetter :: Bool, unsatisfiable :: Bool,
               removeReds :: Bool, keepNops :: Bool, solverFlags :: String,
               outTemp :: Bool, presolver :: Maybe FilePath,
               solver :: Maybe FilePath}
    deriving (Data, Typeable, Show, Eq)

allModes = [import', linearize', extend', augment', model', export', analyze',
            normalize', lint', count', legalize', plot', run']

uniArgs = cmdArgsMode $ modes allModes &= program "uni"
          &= summary "Unison v0, Roberto Castaneda Lozano [rcas@sics.se]"
          &= help "Generate assembly code using constraint programming"

import' = Import {
  targetName      = "Hexagon" &= typ "TARGET" &= explicit &= name "target" &= name "t",
  inFile          = "" &= argPos 1 &= typFile,
  targetOption    = [] &= help "Target-specific option",
  outFile         = Nothing &= name "o" &= help "Output file name" &= typFile,
  debug           = False &= name "d" &= help "Print output of each pass",
  intermediate    = False &= name "i" &= help "Generate intermediate file after each pass",
  lint            = False &= name "l" &= help "Verify that the output is correct by invoking the lint tool",
  lintPragma      = True &= help "Embed lint pragma in Unison IR to disable specific tests",
  estimateFreq    = False &= help "Estimate block execution frequency if it is not provided",
  simplifyControlFlow = True &= help "Simplify the function's control flow",
  implementFrames = True &= help "Implement stack frame handling",
  noCC            = False &= help "Do not enforce calling convention",
  noReserved      = False &= help "Do not enforce reserved registers",
  maxBlockSize    = Nothing &= help "Maximum block size",
  function        = Nothing &= help "Name of the function to import from the input MachineIR",
  goal            = Nothing &= help "Optimization goal (one of {speed, size})"}
  &= help "Import a MachineIR function into Unison"

linearize' = Linearize {} &= help "Transform a Unison function into Linear SSA form"

extend' = Extend {} &= help "Extend a Unison function with copies"

augment' = Augment {
  noCross  = False &= help "Do not cross temporary flow",
  oldModel = False &= help "Extend operands as for the old CP2012 model",
  expandCopies = True &= help "Expand copies in a target-dependent manner",
  rematerialize = True &= help "Add rematerialization decisions whenever suitable"}
  &= help "Augment a Unison function with alternative temporaries"

model' = Model {
  baseFile           = Nothing &= help "Base assembly solution" &= typFile,
  scaleFreq          = True  &= help "Scale down block frequencies if there is a potential cost function overflow",
  applyBaseFile      = True &= help "Apply base file to limit the maximum cost of the function",
  tightPressureBound = False &= help "Compute a tight bound of the register atoms contained in an infinite register space (incompatible with presolver's infinite register dominance constraints)",
  strictlyBetter     = True &= help "Require the solver to find a strictly better solution than the base (as opposed to better or equal)",
  unsatisfiable      = False &= help "Make the constraint problem trivially unsatisfiable"}
  &= help "Generate a code generation problem for a Unison function"

export' = Export {
  removeReds = False &= help "Remove redundant instructions as a post-code generation optimization",
  keepNops   = False &= help "Keep non-critical NOP instructions",
  baseFile   = Nothing &= help "Base assembly solution" &= typFile,
  solFile    = Nothing &= help "Solution file" &= typFile}
  &= help "Export a Unison solution as a MachineIR function"

analyze' = Analyze {
  goals          = "" &= help "Comma-separated list of goals (speed, size) to analyze",
  simulateStalls = True &= help "Simulate stalls due to unsatisfied non-critical latencies",
  modelCost      = False &= help "Compute costs as similarly as possible to the model cost function"}
  &= help "Analyze a MachineIR function"

normalize' = Normalize {} &= help "Normalize a MachineIR function"

lint' = Lint {
  allTemporariesDefined    = True,
  singleDefinitions        = True,
  allTemporariesUsed       = True,
  allRegistersDefined      = True,
  noReadWriteOverlaps      = True,
  allRegClassesDefined     = True,
  noEmptyRegClass          = True,
  consistentOperandInfo    = True,
  consistentOperands       = True,
  consistentPreAssignments = True,
  noRedefinitions          = True,
  noEdgeInterferences      = True,
  noMustConflicts          = True,
  noCongruentCopy          = True,
  noIsolatedGlobals        = True,
  uniqueOperationIds       = True,
  uniqueOperandIds         = True,
  singleEntryBlock         = True,
  allEntryOpsPreAssigned   = True,
  allExitOpsPreAssigned    = True,
  noComponentConflicts     = True,
  noCostOverflow           = True,
  noAmbiguousPhis          = True,
  allResourcesDefined      = True,
  allRegClassesReal        = True,
  noReservedRegRedef       = True,
  noEmptyBlock             = True
}
  &= help "Lint a Unison function"

count' = Count {
  singleRow = False &= help "Print all statistics in a row, without labels"}
  &= help "Collect statistics of a Unison function"

legalize' = Legalize {} &= help "Adapt a Unison function to the form required by the toolchain"

plot' = Plot {
  genBcfg     = False &= help "Generate a block control-flow graph",
  genIcfg     = False &= help "Generate an instruction control-flow graph",
  genBdt      = False &= help "Generate a block dominance tree",
  genDg       = False &= help "Generate a dependency graph",
  genCg       = False &= help "Generate a coalescing graph",
  genSg       = False &= help "Generate a congruence graph",
  genOg       = False &= help "Generate an operand graph",
  genPg       = False &= help "Generate a precedence graph",
  genPpg      = False &= help "Generate a precedence graph with positive, mandatory precedences only",
  graphBlock  = Nothing &= help "Block for which the graph should be generated",
  simpleGraph = False &= help "Generate simplified version"}
  &= help "Plot a Unison function in GraphViz format"

run' = Run {
  verbose     = False &= name "v" &= help "Run Unison in verbose mode",
  solverFlags = "" &= help "Flags to be passed on to the solver",
  outTemp     = False &= help "Dump output into temporary file with same prefix as the generated intermediate files",
  presolver   = Nothing &= help "Path to Unison's presolver binary" &= typFile,
  solver      = Nothing &= help "Path to Unison's solver binary" &= typFile}
  &= help "Run the entire Unison toolchain from 'uni import' to 'uni export'"
