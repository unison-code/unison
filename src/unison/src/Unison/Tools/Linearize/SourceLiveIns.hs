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
import Data.Maybe

import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.ICFG as ICFG

sourceLiveIns f @ Function {fCode = code, fCongruences = cs,
                            fRematerializable = rts} target =
    let bif      = branchInfo target
        icfg     = (ICFG.fromBCFG . BCFG.fromFunction bif) f
        codeSame = foldWithTempIndex (srcLiveIns icfg) [] code
        code'    = map fst codeSame
        newCs    = concatMap (map (mapTuple undoPreAssign) . snd) codeSame
        cs'      = cs ++ newCs
        newRts   = mapMaybe (congruenceToRematTuple (M.fromList rts)) newCs
        rts'     = nub $ rts ++ newRts
    in f {fCode = code', fCongruences = cs', fRematerializable = rts'}

srcLiveIns icfg (i, codeSame) b @ Block {bLab = l, bCode = code} =
  let ts          = nub (sort (tUses code))
      -- We do not need to consider all temporaries defined in dominant blocks,
      -- since the sinkLiveOuts transformation has taken care of that.
      liveIns     = filter (ICFG.isLiveIn (BCFG.toNode l) icfg) ts
      same        = zip liveIns (map mkTemp [i..])
      sCode       = addToIn (map snd same) code
      sameIds     = M.fromList $ map (mapTuple tId) same
      renamedCode = map (mapToModelOperand (applyTempIdMap sameIds)) sCode
  in (i + fromIntegral (length same),
      codeSame ++ [(b {bCode = renamedCode}, same)])

congruenceToRematTuple rts (t, t') =
  case M.lookup t rts of
   Just oids -> Just (t', oids)
   Nothing -> Nothing
