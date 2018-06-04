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
module Unison.Tools.Import.LiftRegs (liftRegs) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Unison
import Unison.Target.API
import Unison.Graphs.Hoopl
import Unison.Graphs.Hoopl.ReachingDefinitions
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.ICFG as ICFG

-- TODO: update targets to work with new '--explicitcallregs' flag, then get rid
-- of ExtractCallRegs pass (this pass should do all register lifting work).

-- TODO: check that there should not be any 'orphan' after this pass

liftRegs f @ Function {fCode = code} target =
    let rr    = map (mkRegister . mkTargetRegister) $ reserved target
        lr    = isLiftableRegister rr
        bif   = branchInfo target
        icfg  = ICFG.fromBCFG $ BCFG.fromFunction bif f
        cfg   = toHOprGraph icfg
        o2ds  = reachingDefinitions lr cfg
        rds   = sort $ nub $ concatMap S.toList $ M.elems o2ds
        tid   = newTempIndex (flatten code)
        rd2t  = M.fromList $ zip rds [tid..]
        code' = mapToOperationInBlocks (renameRegs o2ds rd2t) code
    in f {fCode = code'}

isLiftableRegister rr r = isRegister r && not (r `elem` rr)

renameRegs o2ds rd2t o =
    let ds = S.toList $ o2ds M.! oId o
    in mapToOperands
           (map (renameUse o ds rd2t))
           (map (renameDef o rd2t)) o

renameUse o ds rd2t u =
    case filter (\rd -> rdVariable rd == u) ds of
      [rd] ->
        let t = mkTemp $ rd2t M.! rd
        in if isPreAssignable o then preAssign t u else t
      [] -> u
      _ -> error ("multiple defs reach a register use: need phi functions!")

renameDef o rd2t d =
    let rd = ReachingDefinition d (oId o)
    in case M.lookup rd rd2t of
         Just tid ->
             let t = mkTemp tid
             -- TODO: how do we handle pre-assignments, in general?
             in if isPreAssignable o then preAssign t d else t
         Nothing -> d

isPreAssignable o = isIn o || isOut o || isFun o
