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
module Unison.Tools.Import.AdjustPhiLabels (adjustPhiLabels) where

import Data.Graph.Inductive

import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG
import Unison.Graphs.Util

adjustPhiLabels f @ Function {fCode = code} target =
    let bif   = branchInfo target
        bcfg  = BCFG.fromFunction bif f
        code' = map (adjustPhiLabelsInBlock bcfg) code
    in f {fCode = code'}

adjustPhiLabelsInBlock bcfg b @ Block {bLab = l, bCode = code} =
  b {bCode = map (adjustPhiLabels' bcfg l) code}

adjustPhiLabels' bcfg l
    SingleOperation {oId = id,
                      oOpr = (Virtual Phi {oPhiD = d, oPhiUs = us})} =
      let us' = map (adjustPhiLabel bcfg l) us
      in mkPhi id us' d

adjustPhiLabels' _ _ i = i

adjustPhiLabel bcfg phiB (BlockRef predB) =
  BlockRef (immediateNonreflexivePredecessor bcfg phiB predB)
adjustPhiLabel _ _ t | isTemporary t = t

-- | Gives the immediate predecessor of phiB coming from predB where a node is
-- not predecessor of itself unless it has a loop edge.
immediateNonreflexivePredecessor bcfg phiB predB =
  case BCFG.immediatePredecessor bcfg phiB predB of
    phiB' | phiB' == phiB && phiB == predB ->
      immediatePredecessorForLoop bcfg phiB
    phiB'                   -> phiB'

immediatePredecessorForLoop bcfg b | isLoop bcfg (BCFG.toNode b) = b
immediatePredecessorForLoop bcfg b =
  let bId    = BCFG.toNode b
      succs  = map BCFG.fromNode $ suc bcfg bId
      (rs:_) = filter (\s -> bId `elem` reachable (BCFG.toNode s) bcfg) succs
  in BCFG.immediatePredecessor bcfg b rs