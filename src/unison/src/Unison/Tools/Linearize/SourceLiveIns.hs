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
module Unison.Tools.Linearize.SourceLiveIns (sourceLiveIns) where

import Data.List
import qualified Data.Map as M
import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.ICFG as ICFG

sourceLiveIns f @ Function {fCode = code, fCongruences = cs} target =
    let bif       = branchInfo target
        icfg     = (ICFG.fromBCFG . BCFG.fromFunction bif) f
        codeSame = foldWithTempIndex (srcLiveIns icfg) [] code
        code'    = map fst codeSame
        cs'      = cs ++ concatMap (map clearPreAssignments . snd) codeSame
    in f {fCode = code', fCongruences = cs'}

srcLiveIns icfg (i, codeSame) b @ Block {bLab = l, bCode = code} =
  let ts          = nub (sort (tUses code))
      -- We do not need to consider all temporaries defined in dominant blocks,
      -- since the sinkLiveOuts transformation has taken care of that.
      liveIns     = filter (ICFG.isLiveIn (BCFG.toNode l) icfg) ts
      same        = zip liveIns (map mkTemp [i..])
      sCode       = addToIn (map snd same) code
      sameIds     = M.fromList [(tId t, tId t') | (t, t') <- same]
      renamedCode = map (mapToModelOperand (applyTempIdMap sameIds)) sCode
  in (i + fromIntegral (length same),
      codeSame ++ [(b {bCode = renamedCode}, same)])

clearPreAssignments (t, t') = (undoPreAssign t, undoPreAssign t')
