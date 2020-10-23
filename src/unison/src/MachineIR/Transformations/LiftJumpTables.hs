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
module MachineIR.Transformations.LiftJumpTables (liftJumpTables) where

import Data.List
import Data.Maybe

import MachineIR

import Unison.Target.API
import Unison.Base
import Unison.Util
import Unison.Instances()

liftJumpTables mf @ MachineFunction {mfBlocks = mbs} target =
  let itf   = instructionType target
      bif   = branchInfo target
      oif   = operandInfo target
      mbs'  = map (liftJumpTablesInBlock (itf, bif, oif)) mbs
      mbs'' = map removeBlockSuccs mbs'
      mf'   = mf {mfBlocks = mbs''}
  in mf'

liftJumpTablesInBlock fs mb @ MachineBlock {mbProperties   = mps,
                                            mbInstructions = mis} =
  case find isMachineBlockPropertySuccs mps of
    -- Block successors are only required to support jump tables
    Nothing -> mb
    (Just ps) ->
      let succs = map fst $ mbPropertySuccs ps
          mis'  = map (liftJumpTablesInInstruction fs succs) mis
      in mb {mbInstructions = mis'}

liftJumpTablesInInstruction fs succs mi @ MachineBundle {mbInstrs = mis} =
  mi {mbInstrs = map (liftJumpTablesInInstruction fs succs) mis}
liftJumpTablesInInstruction (itf, bif, oif) succs mi
    | isMachineBranch itf mi =
        let o = fromMachineInstruction itf oif (-1, mi)
        in case fromJust (bif o) of
          (BranchInfo _ Nothing) | not (null succs) ->
            let scs  = map mkMachineBlockRef succs
                mjbs = mkMachineInstructionPropertyJTIBlocks scs
                mi'  = mi {msProperties = msProperties mi ++ [mjbs]}
            in mi'
          _ -> mi
    | otherwise = mi

removeBlockSuccs mb @ MachineBlock {mbProperties = mps} =
  let mps' = filter (not . isMachineBlockPropertySuccs) mps
  in mb {mbProperties = mps'}
