{-|
Copyright   :  Copyright (c) 2019, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module SpecsGen.RegisterClassDeclGen (emitRegisterClassDecl) where

import Data.Maybe
import qualified Data.Map as M
import Data.List

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitRegisterClassDecl targetName is =
    let ids = sort $ nub $ concatMap iRegClasses is
    in [hsModule
        (moduleName targetName (targetName ++ "RegisterClassDecl"))
        (Just [hsExportDataType (targetName ++ "RegisterClass")])
        []
        [hsRegClassDecl targetName ids]]

iRegClasses i = mapMaybe regClass $ M.toList $ mkOperandMap $ iOperands i

regClass (_, (YRegisterInfo _ rc _)) = Just rc
regClass _ = Nothing
