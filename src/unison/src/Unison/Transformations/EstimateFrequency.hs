{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Transformations.EstimateFrequency (estimateFrequency) where

import Data.Maybe
import qualified Data.Map as M

import Unison.Base
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.DT as DT
import Unison.Analysis.FrequencyEstimation

estimateFrequency f @ Function {fCode = code} target =
  let bif    = branchInfo target
      bcfg  = BCFG.fromFunction bif f
      bdt   = DT.fromCFG bcfg
      freq  = fromNaturalLoops loopFactor bcfg bdt
      code' = map (fillBlockFreq freq) code
  in f {fCode = code'}

fillBlockFreq freq b @ Block {bLab = l, bAs = attrs}
  | isJust (aFreq attrs) = b
  | otherwise = b {bAs = (attrs {aFreq = Just (freq M.! l)})}

-- | Estimated average number of loop iterations
loopFactor = 10
