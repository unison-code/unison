{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>
This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module SpecsGen.Driver (SpecsGen(..), writeHsFile, runSpecsGen) where

import System.FilePath
import System.Console.CmdArgs
import Data.Yaml
import qualified Data.ByteString.Char8 as B8
import Language.Haskell.Pretty
import Data.Maybe

import SpecsGen.SimpleYaml
import SpecsGen.HsGen
import SpecsGen.OperandInfoGen
import SpecsGen.AlignedPairsGen
import SpecsGen.InstructionDeclGen
import SpecsGen.ReadOpGen
import SpecsGen.ShowInstanceGen
import SpecsGen.ReadWriteInfoGen
import SpecsGen.ItineraryGen
import SpecsGen.SizeGen
import SpecsGen.InstructionTypeGen
import SpecsGen.AllInstructionsGen
import SpecsGen.ItineraryDeclGen
import SpecsGen.ParentGen

data SpecsGen =
    SpecsGen {files :: [FilePath], targetName :: String, outputDir :: String,
              constantExtend :: Bool, infiniteRegClass :: [String],
              abstractRegClass :: [String], promoteEffect :: [String],
              regClass :: [String], rematFile :: Maybe FilePath}
    deriving (Data, Typeable, Show)

specsgen = cmdArgsMode $ SpecsGen
           {
             files = def &= args &= typ "FILES",
             targetName = "",
             outputDir = "" &= typFile,
             constantExtend = False,
             infiniteRegClass = [],
             abstractRegClass = [],
             promoteEffect = [],
             regClass = [],
             rematFile = Nothing
           }
    &= summary "Generates partial Haskell files (.hs) with target information from the given YAML description (.yaml)\nRoberto Castaneda Lozano roberto.castaneda@ri.se"

runSpecsGen tPreMod tExtension =
    do sg @ SpecsGen{..} <- cmdArgsRun specsgen
       yaml  <- mapM readFile files
       remat <- maybe (return "") readFile rematFile
       let is   = concatMap yamlInstructions yaml
           is1  = expand is
           is2  = is1 ++ extendRemats is1 (yamlInstructions remat)
           is3  = is2 ++
                  if constantExtend then mapMaybe constantExtendedOperation is2
                  else []
           is4 = map (promote promoteEffect) is3
           is5 = map (update regClass) is4
           is6 = tPreMod is5 -- Hand off yaml to target for target-specific modifications
           abstractRegClass' = abstractRegClass ++ ["Unknown"]
       writeHsFile outputDir "OperandInfo"
         (emitOperandInfo targetName (infiniteRegClass, abstractRegClass') is6)
       writeHsFile outputDir "AlignedPairs" (emitAlignedPairs targetName is6)
       writeHsFile outputDir (targetName ++ "InstructionDecl") (emitInstructionDecl targetName is6)
       writeHsFile outputDir "ReadOp" (emitReadOp targetName is6)
       writeHsFile outputDir "ShowInstance" (emitShowInstance targetName is6)
       writeHsFile outputDir "ReadWriteInfo" (emitReadWriteInfo targetName is6)
       writeHsFile outputDir "Itinerary" (emitItinerary targetName is6)
       writeHsFile outputDir "Size" (emitSize targetName is6)
       writeHsFile outputDir "InstructionType" (emitInstructionType targetName is6)
       writeHsFile outputDir "AllInstructions" (emitAllInstructions targetName is6)
       writeHsFile outputDir (targetName ++ "ItineraryDecl") (emitItineraryDecl targetName is6)
       writeHsFile outputDir "Parent" (emitParent targetName is6)
       tExtension sg is6

writeHsFile dir base f =
    writeFile (dir </> addExtension base ".hs")
    (topComment ++ concatMap (\d -> prettyPrint d ++ "\n\n") f)

yamlInstructions = yInstructions . simplify . decodeYaml

topComment = "-- This file has been generated by specsgen. Do not modify by hand!\n\n"

decodeYaml :: String -> Value
decodeYaml s =
  case decodeEither $ B8.pack s of
    (Left err) -> error err
    Right yaml -> yaml
