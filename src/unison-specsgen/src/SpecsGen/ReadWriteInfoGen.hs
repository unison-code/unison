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
module SpecsGen.ReadWriteInfoGen (emitReadWriteInfo) where

import Language.Haskell.Syntax

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitReadWriteInfo targetName is =
    let us2ids = infoToIds iReadWriteInfo is
        rhss   = map (mkOpcRhs idToHsCon toReadWriteRhs) us2ids
    in [hsModule
        (moduleName targetName "ReadWriteInfo")
        (Just [hsExportVar "readWriteInfo"])
        [unisonImport, instructionDeclImport targetName,
         registerDeclImport targetName]
        [simpleOpcFunBind "readWriteInfo" rhss]]

iReadWriteInfo i =
    let rs = toAffectsList $ iAffectedBy i
        ws = toAffectsList $ iAffects i
    in (rs, ws)

toReadWriteRhs (ri, wi) =
    let riTup = map toReadWriteInfoExp ri
        wiTup = map toReadWriteInfoExp wi
    in HsTuple [HsList riTup, HsList wiTup]

toReadWriteInfoExp (YMemory m) = (HsApp (toHsCon "Memory") (toHsStr m))
toReadWriteInfoExp (YOtherSideEffect e) =
  (HsApp (toHsCon "OtherSideEffect") (toHsCon (toOpType e)))
toReadWriteInfoExp other = error ("unmatched: toReadWriteInfoExp " ++ show other)
