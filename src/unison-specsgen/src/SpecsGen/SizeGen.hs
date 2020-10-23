{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module SpecsGen.SizeGen (emitSize) where

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitSize targetName is =
    let us2ids = infoToIds iSize is
        rhss   = map (mkOpcRhs idToHsCon toSizeRhs) us2ids
    in [hsModule
        (moduleName targetName "Size")
        (Just [hsExportVar "size"])
        [instructionDeclImport targetName]
        [simpleOpcFunBind "size" rhss]]

toSizeRhs = toHsInt
