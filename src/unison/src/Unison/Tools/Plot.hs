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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Unison.Tools.Plot (run) where

import Data.Maybe
import System.FilePath
import Control.Monad
import Data.GraphViz.Commands.IO

import Unison
import Unison.Parser
import Unison.Target.API
import Unison.Target.Query
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.ICFG as ICFG
import qualified Unison.Graphs.DT as DT
import qualified Unison.Graphs.DG as DG
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.OG as OG
import qualified Unison.Graphs.PG as PG

run (genBcfg, genIcfg, genBdt, genDg, genCg, genSg, genOg, genPg, genPpg,
     graphBlock, simpleGraph, uniFile) uni target =
  let bif      = branchInfo target
      rwlf    = readWriteLatency target
      oif     = operandInfo target
      apf     = alignedPairs target
      rm      = resourceManager target
      f       = parse target uni
      bcfg    = BCFG.fromFunction bif f
      icfg    = ICFG.fromBCFG bcfg
      bdt     = DT.fromCFG bcfg
      dg      = case graphBlock of
        Nothing  -> DG.fromFunction rwlf rm oif f
        (Just b) -> DG.fromBlock rwlf rm oif (fCode f !! b)
      cg      = case graphBlock of
        Nothing  -> CG.fromFunction f
        (Just b) -> CG.fromBlock (fCode f !! b) (fCongruences f)
      sg      = SG.fromFunction (Just apf) f
      og      = case graphBlock of
        Nothing  -> OG.fromFunction f
        (Just b) -> OG.fromBlock (fCode f !! b) (fCongruences f)
      pg      = PG.fromDependencyGraph oif dg
      ppg     = PG.mandatory $ PG.positive $ PG.fromDependencyGraph oif dg
      outBcfg = BCFG.toDot bcfg (fCongruences f) simpleGraph
      outIcfg = ICFG.toDot icfg
      outBdt  = BCFG.toDot bdt (fCongruences f) simpleGraph
      outDg   = DG.toDot dg simpleGraph
      outCg   = CG.toDot cg
      outSg   = (if isAugmented f then SG.toOperandDot else SG.toTempDot) sg
      outOg   = OG.toDot og
      outPg   = PG.toDot pg
      outPpg  = PG.toDot ppg
      baseName = takeBaseName uniFile
  in do when genBcfg $ writeGraphFile baseName "b.cfg" outBcfg
        when genIcfg $ writeGraphFile baseName "i.cfg" outIcfg
        when genBdt  $ writeGraphFile baseName "d.cfg" outBdt
        when genDg   $ writeGraphFile baseName (graphExt graphBlock "dg") outDg
        when genCg   $ writeGraphFile baseName (graphExt graphBlock "cg") outCg
        when genSg   $ writeGraphFile baseName "sg" outSg
        when genOg   $ writeGraphFile baseName (graphExt graphBlock "og") outOg
        when genPg   $ writeGraphFile baseName (graphExt graphBlock "pg") outPg
        when genPpg  $ writeGraphFile baseName (graphExt graphBlock "ppg") outPpg

writeGraphFile base g = writeDotFile (addExtension base ("." ++ g ++ ".dot"))

graphExt graphBlock t = (t ++ (if isJust graphBlock then ".block" else ""))
