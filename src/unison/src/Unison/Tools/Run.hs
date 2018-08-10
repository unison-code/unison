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
module Unison.Tools.Run (run) where

import System.IO
import System.Directory
import System.Process
import Control.Monad
import Data.List.Split
import Data.Maybe

import Common.Util
import Unison.Driver
import qualified Unison.Tools.Import as Import
import qualified Unison.Tools.Linearize as Linearize
import qualified Unison.Tools.Extend as Extend
import qualified Unison.Tools.Augment as Augment
import qualified Unison.Tools.Model as Model
import qualified Unison.Tools.Export as Export
import qualified Unison.Tools.Normalize as Normalize
import MachineIR.Parser

run args @
  (_estimateFreq, _simplifyControlFlow, _noCC, _noReserved, _maxBlockSize,
   _implementFrames, _function, _goal, _noCross, _oldModel, _expandCopies,
   _rematType, baseFile, _scaleFreq, _applyBaseFile, _tightPressureBound,
   _strictlyBetter, _unsatisfiable, _removeReds, _keepNops, _presolverFlag,
   _solverFlag, mirVersion, inFile, _debug, _verbose, _intermediate, _lint,
   outFile, outTemp, cleanTemp, _presolver, _solver, _sizeThreshold,
   _explicitCallRegs)
  targetWithOption =
  do mirInput         <- strictReadFile inFile
     maybeAsmMirInput <- maybeStrictReadFile baseFile
     let mirInputs    = splitDocs mirVersion mirInput
         asmMirInputs =
           case maybeAsmMirInput of
            Just asmMirInput -> map Just $ splitDocs mirVersion asmMirInput
            Nothing          -> replicate (length mirInputs) Nothing
     prefixes <- mapM (runFunction args targetWithOption)
                      (zip mirInputs asmMirInputs)
     unisonMirOutputs <- mapM (strictReadFile . addExtension "unison.mir")
                         prefixes
     when cleanTemp $
       mapM_ removeFileIfExists (concatMap addUnisonExtensions prefixes)

     prefix <- getTempPrefix
     let unisonMirFile =
           case outFile of
            Just file -> Just file
            Nothing -> if outTemp then Just (addExtension "unison.mir" prefix)
                       else Nothing
         unisonMirOutput =
           case mirInputs of
             -- empty module: return IR (should be the same as asmMirInput)
             [] -> mirInput
             _  -> combineDocs mirVersion
                   (concatMap (splitDocs mirVersion) unisonMirOutputs)
     emitOutput unisonMirFile unisonMirOutput
     when cleanTemp $ removeFileIfExists prefix
     return prefixes

runFunction
  (estimateFreq, simplifyControlFlow, noCC, noReserved, maxBlockSize,
   implementFrames, function, goal, noCross, oldModel, expandCopies, rematType,
   _baseFile, scaleFreq, applyBaseFile, tightPressureBound, strictlyBetter,
   unsatisfiable, removeReds, keepNops, presolverFlag, solverFlag, mirVersion,
   inFile, debug, verbose, intermediate, lint, _outFile, _cleanTemp, _outTemp,
   presolver, solver, sizeThreshold, explicitCallRegs)
  targetWithOption (mir, asmMir) =
  do prefix <- getTempPrefix
     let maybePutStrLn = when verbose . hPutStrLn stderr
         lintPragma    = True
         concatFun     = \(rawIR, rawMir) -> rawIR ++ rawMir
         mirInput      = concatFun mir
         asmMirInput   = fmap concatFun asmMir
     maybePutStrLn ("Running function with prefix '" ++ prefix ++ "'...")

     baseFile' <- normalize
                  (prefix, estimateFreq, simplifyControlFlow, mirVersion,
                   debug, verbose, targetWithOption) asmMirInput

     let uniFile = addExtension "uni" prefix
     maybePutStrLn "Running 'uni import'..."
     importResult <-
       Import.run
       (estimateFreq, simplifyControlFlow, noCC, noReserved, maxBlockSize,
        implementFrames, rematType, function, goal, mirVersion, sizeThreshold,
        explicitCallRegs, inFile, debug, intermediate, lint, lintPragma,
        Just uniFile) mirInput targetWithOption

     case importResult of
      -- import succeeds:
      (Right _) ->
        do uniInput <- strictReadFile uniFile

           let lssaUniFile = addExtension "lssa.uni" prefix
           maybePutStrLn "Running 'uni linearize'..."
           Linearize.run
             (uniFile, debug, intermediate, lint, lintPragma, Just lssaUniFile)
             uniInput targetWithOption
           lssaUniInput <- strictReadFile lssaUniFile

           let extUniFile = addExtension "ext.uni" prefix
           maybePutStrLn "Running 'uni extend'..."
           Extend.run
             (lssaUniFile, debug, intermediate, lint, lintPragma, Just extUniFile)
             lssaUniInput targetWithOption
           extUniInput <- strictReadFile extUniFile

           let altUniFile = addExtension "alt.uni" prefix
           maybePutStrLn "Running 'uni augment'..."
           Augment.run
             (implementFrames, noCross, oldModel, expandCopies, rematType,
              extUniFile, debug, intermediate, lint, lintPragma, Just altUniFile)
             extUniInput targetWithOption
           altUniInput <- strictReadFile altUniFile

           let jsonFile = addExtension "json" prefix
           maybePutStrLn "Running 'uni model'..."
           Model.run
             (baseFile', scaleFreq, oldModel, applyBaseFile, tightPressureBound,
              strictlyBetter, unsatisfiable, noCC, mirVersion, Just jsonFile)
             altUniInput targetWithOption

           let extJsonFile   = addExtension "ext.json" prefix
               splitPSFlags  = concatMap (splitOn "=") presolverFlag
               presolverPath = case presolver of
                                Just path -> path
                                Nothing -> "gecode-presolver"
           maybePutStrLn ("Running '" ++ presolverPath ++ "'...")
           callProcess presolverPath
             (["-o", extJsonFile] ++ ["-verbose" | verbose] ++ splitPSFlags ++
              [jsonFile])

           let outJsonFile  = addExtension "out.json" prefix
               splitSvFlags = concatMap (splitOn "=") solverFlag
               solverPath   = case solver of
                                 Just path -> path
                                 Nothing -> "gecode-solver"
           maybePutStrLn ("Running '" ++ solverPath ++ "'...")
           callProcess solverPath
             (["-o", outJsonFile] ++ ["-verbose" | verbose] ++ splitSvFlags ++
              [extJsonFile])

           let unisonMirFile = addExtension "unison.mir" prefix
           maybePutStrLn "Running 'uni export'..."
           Export.run
             (removeReds, keepNops, baseFile', tightPressureBound, mirVersion, debug,
              outJsonFile, Just unisonMirFile)
             altUniInput targetWithOption

           return prefix

      -- import aborted because of different reasons, assume a base file is
      -- given:
      (Left reason) ->
        do when (verbose && reason == OverSizeThreshold) $
                hPutStrLn stderr "Size threshold exceeded, skipping function..."
           let unisonMirFile = addExtension "unison.mir" prefix
           copyFile (fromJust baseFile') unisonMirFile
           return prefix

addExtension ext prefix = prefix ++ "." ++ ext

normalize _ Nothing = return Nothing
normalize (prefix, estimateFreq, simplifyControlFlow, mirVersion,
           debug, verbose, targetWithOption) (Just asmMirInput) =
  do let llvmMirFile = addExtension "llvm.mir" prefix
     when verbose $ hPutStrLn stderr "Running 'uni normalize'..."
     Normalize.run
       (estimateFreq, simplifyControlFlow, mirVersion, debug, Just llvmMirFile)
       asmMirInput targetWithOption
     return (Just llvmMirFile)

getTempPrefix =
  do tmp <- getTemporaryDirectory
     prefix <- unisonPrefixFile tmp
     return prefix

addUnisonExtensions prefix =
  (map (\e -> addExtension e prefix)
   ["uni", "lssa.uni", "ext.uni", "alt.uni", "json", "ext.json", "out.json",
    "llvm.mir", "unison.mir"]) ++
  [prefix]

removeFileIfExists fileName =
  do fileExists <- doesFileExist fileName
     when fileExists (removeFile fileName)
     return ()
