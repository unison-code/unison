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
module MachineIR.Transformations.SplitTerminators (splitTerminators) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.List.Split

import MachineIR

import Unison.Target.API
import Unison.Util

splitTerminators estimateFreq mf target =
  let itf    = instructionType target
      bif    = branchInfo target
      oif    = operandInfo target
      id     = newMachineBlockId mf
      (mf',
       b2bs) = traverseMachineFunction'
               (splitBlock estimateFreq itf) (id, []) mf
      fts    = M.fromList $ fallthroughBlock (mfBlocks mf')
      succs  = M.fromList $
               map (blockSuccessor itf bif oif fts (mbId $ last (mfBlocks mf')))
               (mfBlocks mf')
      mf''   = mf' {mfBlocks = map (adjustPhis b2bs succs) (mfBlocks mf')}
  in mf''

splitBlock _ _ (id, b2bs) b @ MachineBlock {mbInstructions = mis}
  | mis == [] || any isMachineLast mis =
    ([b], (id, b2bs ++ [(mbId b, [mbId b])]))
splitBlock estimateFreq itf (id, b2bs)
  b @ MachineBlock {mbProperties = mps, mbInstructions = mis} =
  let (bmis:bmis') = split (dropInitBlank . keepDelimsR . dropFinalBlank $
                            whenElt (isMachineBranch itf)) mis
      b'   = b {mbInstructions = bmis}
      mps' = if estimateFreq then
               filter (not . isMachineBlockPropertyFreq) mps
             else mps
      bs   = map (\(bId, mis) -> mkMachineBlock bId mps' mis) (zip [id..] bmis')
      id'  = if null bs then id else maximum (map mbId bs) + 1
  in (b':bs, (id', b2bs ++ [(mbId b, map mbId (b:bs))]))

traverseMachineFunction' fun acc mf @ MachineFunction {mfBlocks = blocks} =
  let (blocks', (_, b2bs)) = foldl (traverseMachineBlock' fun) ([], acc) blocks
  in (mf {mfBlocks = blocks'}, M.fromList b2bs)

traverseMachineBlock' fun (accCode, acc) b =
  let (bs, acc') = fun acc b
  in (accCode ++ bs, acc')

blockSuccessor itf bif oif fts lastId
  MachineBlock {mbId = id, mbInstructions = mis} =
    case find (isMachineBranch itf) mis of
      Just j -> let i = fromMachineInstruction itf oif (-1, j)
                in (id, successors bif (fts M.! id) i)
      Nothing -> if id == lastId then (id, []) else (id, [fts M.! id])

adjustPhis b2bs succs b @ MachineBlock {mbId = id, mbInstructions = mis} =
  b {mbInstructions = map (adjustPhi id b2bs succs) mis}

adjustPhi current b2bs succs mi
  | isMachinePhi mi =
    mi {msOperands = map (adjustPhiOperand current b2bs succs) (msOperands mi)}
  | otherwise = mi

adjustPhiOperand current b2bs succs mop @ MachineBlockRef {mbrId = parent} =
  let candidateParents = b2bs M.! parent
      realParent = find (\p -> current `elem` (succs M.! p)) candidateParents
  in mop {mbrId = fromJust realParent}

adjustPhiOperand _ _ _ mop = mop
