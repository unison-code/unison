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
module SpecsGen.ShowInstanceGen (emitShowInstance) where

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitShowInstance targetName is =
    let ids = map oId is
        ss  = [simpleFun (toHsPVar $ toOpType id) "show" (toHsStr id)
                   | id <- ids]
    in [hsModule
        (moduleName targetName "ShowInstance")
        Nothing
        [instructionDeclImport targetName]
        [hsInstDecl targetName ss]]
