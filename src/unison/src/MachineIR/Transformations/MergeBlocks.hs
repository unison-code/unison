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
module MachineIR.Transformations.MergeBlocks (mergeBlocks) where

import Data.List
import qualified Data.Set as S

import Unison
import Unison.Target.API
import MachineIR

mergeBlocks mf @ MachineFunction {mfBlocks = mbs, mfProperties = mps} target =
  let itf = instructionType target
      oif = operandInfo target
      bif = branchInfo target
      fs  = (itf, oif, bif)
      bbs = S.fromList $
            concatMap (branchTargets fs) (flattenMachineFunction mf)
      jbs = S.fromList $ jtTargets (find isMachineFunctionPropertyJumpTable mps)
      rbs = S.union bbs jbs
      mf' = mf {mfBlocks = doMergeBlocks itf rbs mbs}
  in mf'

doMergeBlocks itf rbs
  (mb1 @ MachineBlock {mbInstructions = mis1} :
   mb2 @ MachineBlock {mbId = id2, mbInstructions = mis2} :
   mbs)
  | none (isMachineBranch itf) mis1 && S.notMember id2 rbs =
      mb1 {mbInstructions = []} :
      mb2 {mbInstructions = mis1 ++ mis2} :
      doMergeBlocks itf rbs mbs
doMergeBlocks itf rbs (mb : mbs) = mb : doMergeBlocks itf rbs mbs
doMergeBlocks _ _ [] = []

branchTargets fs MachineBundle {mbInstrs = mis} =
  concatMap (branchTargets fs) mis
branchTargets (itf, oif, bif) mi @ MachineSingle {}
  | isMachineBranch itf mi =
      let o = fromMachineInstruction itf oif (-1, mi)
      in case bif o of
          Just (BranchInfo _ (Just l)) -> [l]
          _ -> []
  | otherwise = []

jtTargets Nothing = []
jtTargets (Just (MachineFunctionPropertyJumpTable _ es)) =
  concatMap (map mbrId . mjtBlocks) es
