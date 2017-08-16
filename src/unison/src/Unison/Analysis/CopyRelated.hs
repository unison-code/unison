{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Algorithms to compute classes of copy-related operands.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.CopyRelated (copyRelatedOperands) where

import Data.List
import Data.Graph.Inductive

import Unison.Base
import Unison.Constructors
import Unison.Predicates
import qualified Unison.Graphs.OG as OG
import qualified Unison.Graphs.Partition as P

copyRelatedOperands :: Ord r => Block i r -> [[Operand r]]
copyRelatedOperands b =
  let og  = OG.fromBlock b []
      og' = elfilter (not . isOperandNaturalEdge) og
      cr  = P.fromGraph og'
  in map (sort . map (mkOperandRef . toInteger)) (P.toList cr)
