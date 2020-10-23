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
