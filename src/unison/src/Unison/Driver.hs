{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Support to build Unison tools.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}

module Unison.Driver
    (ImportSkipReason (..),
     applyTransformations,
     emitOutput,
     toPlainText,
     writeIntermediateFile,
     pickTarget,
     unisonPrefixFile)
    where

import Common.Util

import Data.Maybe
import Data.List
import System.FilePath
import System.IO

data ImportSkipReason = OverSizeThreshold | NotSelected deriving Eq

applyTransformations ts target f = mapAccumL (applyTransformation target) f ts

applyTransformation _ f (_, n, False) = (f, (n, Nothing))
applyTransformation target f (t, n, True) =
    let o   = t f target
        out = (n, Just (showSimple o))
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
     -- the prefix file 'uniFile' is not deleted to prevent the next call to
     -- 'unisonPrefixFile' to create the same prefix, the caller is responsible
     -- for its deletion.
     return uniFile
