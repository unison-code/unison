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
module SpecsGen.InstructionDeclGen (emitInstructionDecl) where

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitInstructionDecl targetName is =
    let ids = map (toOpType . oId) is
    in [hsModule
        (moduleName targetName (targetName ++ "InstructionDecl"))
        (Just [hsExportDataType (targetName ++ "Instruction")])
        []
        [hsDataDecl targetName ids]]
