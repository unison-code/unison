{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Import.CorrectDoubleBranches (correctDoubleBranches) where

import Data.List
import Data.Graph.Inductive
import qualified Data.Set as S
import qualified Data.Map as M

import Common.Util
import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import Unison.Graphs.Util

correctDoubleBranches f @ Function {fCode = code} target =
    let bif   = branchInfo target
        bcfg  = BCFG.fromFunction bif f
        code' = map (correctDoubleBranchesInBlock bcfg) code
    in f {fCode = code'}

correctDoubleBranchesInBlock bcfg b @ Block {bLab = l, bCode = code}
  | hasLoopPhis b && not (isLoop bcfg (BCFG.toNode l)) =
    let [p]    = immediatePredsAndSuccs bcfg l
        labMap = M.fromList [(mkBlockRef l, mkBlockRef p)]
        code'  = mapIf isPhi (replaceBlockRefs labMap) code
    in b {bCode = code'}
  | otherwise = b

replaceBlockRefs :: Ord r => M.Map (Operand r) (Operand r) ->
                    BlockOperation i r -> BlockOperation i r
replaceBlockRefs = mapToOperandIf isBlockRef . applyMap

hasLoopPhis Block {bLab = l, bCode = code} =
  let phis   = filter isPhi code
      predBs = nub $ concatMap (map snd . phiUses) phis
  in elem l predBs

immediatePredsAndSuccs bcfg b =
  let bId    = BCFG.toNode b
      toBSet = S.fromList . map BCFG.fromNode
      preds  = toBSet $ pre bcfg bId
      succs  = toBSet $ suc bcfg bId
  in S.toList $ S.intersection preds succs
