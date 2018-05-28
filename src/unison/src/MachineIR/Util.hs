{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Useful functions to manipulate and query the machine IR program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module MachineIR.Util
       (
         flattenMachineFunction,
         newMachineBlockId,
         newMachineTempId,
         traverseMachineFunction,
         mapToMachineInstruction,
         concatMapToMachineInstructionBlock,
         mapToTargetMachineInstruction,
         concatMapToMachineInstruction,
         mapToMachineOperand,
         mapToMachineBlock,
         mapToMachineBlockId,
         machineBlockFreq,
         filterMachineInstructions,
         filterMachineInstructionsBlock,
         runMachineTransformations,
         expandBlockPseudos,
         miToList,
         listToMi,
         fallthroughBlock,
         registerClassMap
       )
       where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Common.Util

import MachineIR.Base
import MachineIR.Constructors
import MachineIR.Predicates

flattenMachineFunction MachineFunction {mfBlocks = blocks} =
  concatMap flattenMachineBlock blocks
flattenMachineBlock MachineBlock {mbId = id, mbInstructions = is} =
  mkBlockMarker id : is

newMachineBlockId mf = maxZ (map mbId (mfBlocks mf)) + 1

newMachineTempId mf = maxZ [maxTempId mi | mi <- flattenMachineFunction mf] + 1
maxTempId mi = maxZ [id | MachineTemp {mtId = id} <- msOperands mi]

maxZ = maybeMax 0

traverseMachineFunction fun acc mf @ MachineFunction {mfBlocks = blocks} =
  let (blocks', acc') = foldl (traverseMachineBlock fun) ([], acc) blocks
  in (mf {mfBlocks = blocks'}, acc')

traverseMachineBlock fun (accCode, acc) b @ MachineBlock {mbInstructions = is} =
  let (is', acc') = fun ([], acc) is
  in (accCode ++ [b {mbInstructions = is'}], acc')

mapToMachineInstruction f = concatMapToMachineInstruction (toSingleton . f)

mapToTargetMachineInstruction f =
    concatMapToMachineInstruction
    (toSingleton . applyIf (not . isMachineVirtual) f)

concatMapToMachineInstruction =
    mapToMachineBlock . concatMapToMachineInstructionBlock

concatMapToMachineInstructionBlock f mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = concatMap (concatMapToMachineSingle f) mis}

concatMapToMachineSingle f mi @ MachineSingle {} =
    case f mi of
      [mi'] -> [mi']
      mis   -> [mkMachineBundle mis]

concatMapToMachineSingle f mi @ MachineBundle {mbInstrs = mis} =
  [mi {mbInstrs = concatMap (concatMapToBundledMachineSingle f) mis}]

concatMapToBundledMachineSingle f mi @ MachineSingle {} = f mi

mapToMachineOperand f mi =
    fromSingleton $ concatMapToMachineSingle (mapToMachineSingleOperand f) mi

mapToMachineSingleOperand f mi @ MachineSingle {msOperands = mos} =
    [mi {msOperands = map f mos}]

mapToMachineBlock f mf @ MachineFunction {mfBlocks = mbs} =
  mf {mfBlocks = map f mbs}

mapToMachineBlockId p f
  mf @ MachineFunction {mfBlocks = mbs, mfProperties = mps} =
    mf {mfBlocks     = map (mapToMachineBlockIdBlock p f) mbs,
        mfProperties = map (mapToMachineBlockIdProperty f) mps}

mapToMachineBlockIdBlock p f mb @ MachineBlock {mbId = id, mbInstructions = mis} =
  let mis' = map (mapToMachineBlockIdInstr p f) mis
  in mb {mbId = f id, mbInstructions = mis'}

mapToMachineBlockIdInstr p f mi @ MachineBundle {mbInstrs = mis} =
  mi {mbInstrs = map (mapToMachineBlockIdInstr p f) mis}

mapToMachineBlockIdInstr p f mi @ MachineSingle {msOperands = mos,
                                                 msProperties = mps}
  | p mi      = mi {msOperands   = map (mapToMachineBlockIdOperand f) mos,
                    msProperties = mapIf isMachineInstructionPropertyJTIBlocks
                                   (mapToMachineBlockIdPs f) mps}
  | otherwise = mi

mapToMachineBlockIdOperand f mo @ MachineBlockRef {mbrId = id} =
  mo {mbrId = f id}
mapToMachineBlockIdOperand _ mo = mo

mapToMachineBlockIdPs f (MachineInstructionPropertyJTIBlocks bs) =
    MachineInstructionPropertyJTIBlocks $ map (mapToMachineBlockIdOperand f) bs
mapToMachineBlockIdPs _ p = p

mapToMachineBlockIdProperty f
  (jt @ MachineFunctionPropertyJumpTable {mfPropertyJumpTable = es}) =
      jt {mfPropertyJumpTable = map (mapToMachineBlockIdJTE f) es}
mapToMachineBlockIdProperty _ mbp = mbp

mapToMachineBlockIdJTE f e @ MachineJumpTableEntry {mjtBlocks = bs} =
  e {mjtBlocks = map (mapToMachineBlockIdOperand f) bs}

machineBlockFreq =
    mbPropertyFreq . fromJust . find isMachineBlockPropertyFreq . mbProperties

filterMachineInstructions = mapToMachineBlock . filterMachineInstructionsBlock

filterMachineInstructionsBlock f mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = concatMap (filterInstruction f) mis}

filterInstruction f mb @ MachineBundle {mbInstrs = mis} =
  case filter f mis of
     []   -> []
     mis' -> [mb {mbInstrs = mis'}]
filterInstruction f ms @ MachineSingle {} = if f ms then [ms] else []

runMachineTransformations ts mf = foldl (\mf f -> f mf) mf ts

expandBlockPseudos f mi @ MachineBlock {mbInstructions = mis} =
    let mis'   = map miToList mis
        mis''  = expand f mis'
        mis''' = map listToMi mis''
    in mi {mbInstructions = mis'''}

miToList MachineBundle {mbInstrs = mis} = mis
miToList mi @ MachineSingle {}          = [mi]

listToMi [mi] = mi
listToMi mis  = mkMachineBundle mis

fallthroughBlock (MachineBlock {mbId = i1} :
             b2 @ MachineBlock {mbId = i2} : bs) =
                  (i1, i2) : fallthroughBlock (b2 : bs)
fallthroughBlock [MachineBlock {mbId = i1}] = [(i1, -1)]

registerClassMap mf =
  M.fromList $ concat $ maybeToList $ fmap mfPropertyRegisters $
  find isMachineFunctionPropertyRegisters (mfProperties mf)
