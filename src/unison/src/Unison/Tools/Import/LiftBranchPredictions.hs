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
module Unison.Tools.Import.LiftBranchPredictions (liftBranchPredictions) where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import MachineIR

import Unison
import Unison.Target.API

liftBranchPredictions mf @ MachineFunction {mfBlocks = mbs} target =
  let itf  = instructionType target
      bif  = branchInfo target
      oif  = operandInfo target
      mbs' = map (liftBranchPredictionInBlock (itf, bif, oif)) mbs
      mf'  = mf {mfBlocks = mbs'}
  in mf'

liftBranchPredictionInBlock fs mb @ MachineBlock {mbProperties   = mps,
                                                  mbInstructions = mis} =
  case find isMachineBlockPropertySuccs mps of
    Nothing -> mb
    (Just (MachineBlockPropertySuccs mbs)) ->
      let succ = M.fromList mbs
          mis' = map (liftBranchPredictionInInstruction fs succ) mis
      in mb {mbInstructions = mis'}

liftBranchPredictionInInstruction (itf, bif, oif) succ mi
    | isMachineBranch itf mi =
        let o = fromMachineInstruction itf oif (-1, mi)
        in case fromJust (bif o) of
          (BranchInfo Conditional (Just bid)) ->
            let taken = (succ M.! bid) >= 50
                mpt   = mkMachineInstructionPropertyBranchTaken taken
                mi'   = mi {msProperties = msProperties mi ++ [mpt]}
            in mi'
          _ -> mi
    | otherwise = mi
