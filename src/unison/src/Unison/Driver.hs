{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Support to build Unison tools.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}

module Unison.Driver
    (applyTransformations,
     emitOutput,
     toPlainText,
     writeIntermediateFile,
     pickTarget,
     unisonPrefixFile)
    where

import Data.Maybe
import Data.List
import System.FilePath
import System.Directory
import System.IO

applyTransformations ts target f = mapAccumL (applyTransformation target) f ts

applyTransformation _ f (_, n, False) = (f, (n, Nothing))
applyTransformation target f (t, n, True) =
    let o   = t f target
        out = (n, Just (show o))
    in (o, out)

toPlainText :: [(String, Maybe String)] -> String
toPlainText = concatMap outputToPlainText

outputToPlainText (n, Just o)  = "\nOutput from " ++ n ++ ":\n\n" ++ o ++ "\n"
outputToPlainText (n, Nothing) = "(" ++ n ++ " is disabled)\n\n"

writeIntermediateFile _ _ (_, Nothing) = return ()
writeIntermediateFile ext base (passName, Just f) =
  let basePass = base ++ "." ++ passName
  in writeFile (addExtension basePass ("." ++ ext)) f

emitOutput Nothing        = putStr
emitOutput (Just outFile) = writeFile outFile

pickTarget targetName targets =
    fromMaybe (error ("unknown target: " ++ targetName ++
                      " (available targets: " ++ show (map fst targets) ++ ")"))
              (lookup targetName targets)

unisonPrefixFile tmp =
  do (uniFile, h) <- openTempFileWithDefaultPermissions tmp "unison-"
     hClose h
     removeFile uniFile
     return uniFile
