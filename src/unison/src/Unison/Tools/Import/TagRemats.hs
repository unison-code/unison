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
module Unison.Tools.Import.TagRemats (tagRemats) where

import qualified Data.Map as M
import qualified Data.Set as S

import Unison
import Unison.Target.API
import Unison.Graphs.Hoopl
import Unison.Graphs.Hoopl.ReachingConstantsSSA
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.ICFG as ICFG

tagRemats f target =
    let bif  = branchInfo target
        icfg = ICFG.fromBCFG $ BCFG.fromFunction bif f
        cfg  = toHOprGraph icfg
        ts   = map undoPreAssign $ tUniqueOps (flatCode f)
        rts  = [(undoPreAssign t, S.toList oids)
               | (t, (_, oids)) <- M.toList $ reachingConstants cfg ts]
    in f {fRematerializable = rts}
