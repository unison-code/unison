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
module Unison.Tools.Count (run) where

import System.Exit
import System.FilePath
import Data.List
import Data.Maybe
import Text.PrettyPrint

import Common.Util

import Unison hiding (temporaries)
import Unison.Driver
import Unison.Parser
import Unison.Target.API hiding (copies)

run (singleRow, uniFile, outFile) uni target =
  let f     = parse target uni
      stats = map (measureStatistic f target)
              [blocks,
               operations,
               operationsPerBlock,
               maxOperationsInBlock,
               copies,
               operands,
               operandsPerIn,
               maxOperandsInIn,
               operandsPerOut,
               maxOperandsInOut,
               temporaries,
               rematPoints,
               splitBlocks]
      name  = ("Name", takeBaseName uniFile)
  in do emitOutput outFile (showStatistics singleRow (name : stats))
        exitSuccess

measureStatistic f t s = s f t

showStatistics True stats =
    let foo = csWith "" snd stats
    in renderStyle lineStyle foo ++ newLine

showStatistics False stats = unlines (map showStatisticLine stats)

showStatisticLine (name, value) = name ++ " " ++ value

blocks :: Function i r -> TargetWithOptions i r rc s -> (String, String)
blocks f _ = mkStatistic "Blocks" (nBlocks f)
operations f _ = mkStatistic "Operations" (nOperations f)
operationsPerBlock f _ =
    mkStatistic "Operations-per-block"
    (fromIntegral (nOperations f) / fromIntegral (nBlocks f))
maxOperationsInBlock f _ =
    mkStatistic "Maximum-operations-in-block"
    (maxBlockOperations f)
copies f _ = mkStatistic "Copies" (nCopies f)
operands f _ = mkStatistic "Operands" (nOperands always f)
operandsPerIn f _ =
    mkStatistic "Operands-per-in-delimiter"
    (fromIntegral (nOperands isIn f) / fromIntegral (nBlocks f))
maxOperandsInIn f _ =
    mkStatistic "Maximum-operands-in-an-in-delimiter"
    (maxOperands isIn f)
operandsPerOut f _ =
    mkStatistic "Operands-per-out-delimiter"
    (fromIntegral (nOperands isOut f) / fromIntegral (nBlocks f))
maxOperandsInOut f _ =
    mkStatistic "Maximum-operands-in-an-out-delimiter"
    (maxOperands isOut f)
temporaries f _ = mkStatistic "Temporaries" (nTemporaries f)
rematPoints f _ = mkStatistic "Rematerialization-points" (nRematPoints f)
splitBlocks f _ = mkStatistic "Split-blocks" (nSplitBlocks f)

nBlocks = length . fCode
nOperations = length . flatCode
maxBlockOperations f = maxOf (length . bCode) (fCode f)
nCopies = length . filter isCopy . flatCode
nOps = length . oAllOps
nOperands p f = sum [nOps i | i <- flatCode f, p i]
maxOperands p f = maxOf (maxBlockOperands p) (fCode f)
maxBlockOperands p b = nOps $ findSingle p (bCode b)
nTemporaries f = length $ nub $ tUniqueOps (flatCode f)
nRematPoints = length . filter isRemat . flatCode
nSplitBlocks = length . filter isSplitBlock . fCode

maxOf f = maximum . map f
findSingle p ls = fromJust (find p ls)

mkStatistic name value = (name, show value)
