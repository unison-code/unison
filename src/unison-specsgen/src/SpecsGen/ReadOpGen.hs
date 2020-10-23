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
module SpecsGen.ReadOpGen (emitReadOp) where

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitReadOp targetName is =
    let ids = map oId is
    in [hsModule
        (moduleName targetName "ReadOp")
        (Just [hsExportVar "readOp"])
        [instructionDeclImport targetName]
        ([simpleFun (toHsPStr id) "readOp" (toHsCon $ toOpType id) | id <- ids] ++
         [simpleErrorRhs "readOp"])]
