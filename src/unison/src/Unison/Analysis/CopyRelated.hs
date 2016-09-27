{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Algorithms to compute classes of copy-related operands.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.CopyRelated (copyRelatedOperands) where

import Data.List

import Unison.Base
import Unison.Constructors
import Unison.Predicates
import Unison.Graphs.Util
import qualified Unison.Graphs.OG as OG
import qualified Unison.Graphs.Partition as P

copyRelatedOperands :: Ord r => Block i r -> [[Operand r]]
copyRelatedOperands b =
  let og  = OG.fromBlock b []
      og' = labFilter (not . isOperandNaturalEdge) og
      cr  = P.fromGraph og'
  in map (sort . map (mkOperandRef . toInteger)) (P.toList cr)
