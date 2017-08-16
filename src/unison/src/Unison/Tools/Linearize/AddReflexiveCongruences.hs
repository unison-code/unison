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
module Unison.Tools.Linearize.AddReflexiveCongruences
       (addReflexiveCongruences) where

import Data.List

import Unison
import Unison.Target.API
import Unison.Graphs.BCFG
import Unison.Graphs.Util

addReflexiveCongruences f @ Function {fCode = code, fCongruences = cs} target =
    let bcfg = fromFunction (branchInfo target) f
        cs'  = reflexiveCongruenceTuples bcfg code
        cs'' = sort $ cs ++ cs'
    in f {fCongruences = cs''}

reflexiveCongruenceTuples bcfg code =
    concat [inOutPairs bbCode | Block {bLab = l, bCode = bbCode} <- code,
            isLoop bcfg (fromInteger l)]

inOutPairs code =
    let ds = concat [oDefs i | i <- code, isIn i]
        us = concat [oUses i | i <- code, isOut i]
    in [(u, d) | d <- ds, u <- us, d == u]
