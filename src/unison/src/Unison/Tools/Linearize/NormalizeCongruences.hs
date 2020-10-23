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
module Unison.Tools.Linearize.NormalizeCongruences (normalizeCongruences) where

import Data.List
import qualified Data.Set as S

import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.SG as SG

normalizeCongruences f @ Function {fCongruences = cs} target =
    let bif     = branchInfo target
        bcfg   = BCFG.fromFunction bif f
        sg     = SG.fromCongruences cs
        p      = map S.fromList $ SG.sameTempPartitions sg
        cs'    = BCFG.eqvNeighborTemps bcfg p
        cs''   = sort cs'
    in f {fCongruences = cs''}
