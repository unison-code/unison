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
module SpecsGen.AlignedPairsGen (emitAlignedPairs) where

import Data.List
import Language.Haskell.Syntax

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitAlignedPairs targetName is =
    let ud2ids = infoToIds iUseDefs is
    in [hsModule
        (moduleName targetName "AlignedPairs")
        (Just [hsExportVar "alignedPairs"])
        [instructionDeclImport targetName]
        (map emitAlignedPairsForUseDefPattern ud2ids ++ [defaultPairs])]

emitAlignedPairsForUseDefPattern ((us, ds), ids) =
    let os   = (map toVarName us, map toVarName ds)
        rhss = (toPatternTuple os,
                [mkOpcRhs idToHsCon toAlignedPairsRhs (os, ids)])
    in opcOpsFunBind "alignedPairs" rhss

toPatternTuple (l1, l2) =
    let cpt  = l1 `intersect` l2
        cpt' = cpt ++ map toRedefName cpt
        vwf  = \l -> if l `elem` cpt' then toHsPVar l else HsPWildCard
        pl1  = map vwf l1
        pl2  = map (vwf . redefNameIfIn l1) l2
    in (HsPList pl1, HsPList pl2)

toAlignedPairsRhs (us, ds) =
    let cpt = us `intersect` ds
        cp  = [HsTuple [toHsVar t, toHsVar (toRedefName t)] | t <- cpt]
    in HsList cp

defaultPairs = hsFun "alignedPairs" [toHsPVar "_", toHsPVar "_"] (HsList [])
