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
module SpecsGen.ParentGen (emitParent) where

import Language.Haskell.Syntax

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitParent targetName is =
    let us2ids = infoToIds iParent is
        rhss   = map (mkOpcRhs idToHsCon toParentRhs) us2ids
    in [hsModule
        (moduleName targetName "Parent")
        (Just [hsExportVar "parent"])
        [instructionDeclImport targetName]
        [simpleOpcFunBind "parent" rhss]]

toParentRhs Nothing = toHsCon "Nothing"
toParentRhs (Just p) = (HsApp (toHsCon "Just") (toHsCon $ toOpType p))
