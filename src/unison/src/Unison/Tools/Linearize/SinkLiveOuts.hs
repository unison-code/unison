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
module Unison.Tools.Linearize.SinkLiveOuts (sinkLiveOuts) where

import qualified Data.Map as M
import qualified Data.Set as S

import Unison
import Unison.Target.API
import Unison.Graphs.Hoopl
import Unison.Graphs.Hoopl.Liveness

sinkLiveOuts f @ Function {fCode = code} target =
    let bif    = branchInfo target
        cfg   = toHGraph bif f
        b2ts  = liveTemps cfg
        code' = map (sinkLiveOutsInBlock b2ts) code
    in f {fCode = code'}

-- | Sink each temporary defined in a block that is live out
sinkLiveOutsInBlock s2ts b @ Block {bLab = l, bCode = code} =
    b {bCode = addToOut (S.toList $ snd $ s2ts M.! l) code}
