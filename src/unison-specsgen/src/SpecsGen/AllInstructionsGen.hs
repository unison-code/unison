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
module SpecsGen.AllInstructionsGen (emitAllInstructions) where

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitAllInstructions targetName is =
    let ids = map (toOpType . oId) is
    in [hsModule
        (moduleName targetName "AllInstructions")
        (Just [hsExportVar "allInstructions"])
        [instructionDeclImport targetName]
        [constantFun "allInstructions" (toInstructionsList ids)]]

toInstructionsList = toHsList . map toHsCon
