{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

Contributing authors:
  Patric Hedlin <patric.hedlin@ericsson.com>
  Daniel Lundén <daniel.lunden@sisc.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Model.InstructionScheduling (parameters) where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M
import Data.Aeson (toJSON)

import Unison
import Unison.Target.API
import Unison.Target.Query
import qualified Unison.Graphs.DG as DG
import Unison.Analysis.MakespanBounds

import Unison.Tools.Model.Definitions

parameters scaleFreq (_, dgs, _, _, _) Function {fCode = code} target =
  let oif           = operandInfo target
      rm            = resourceManager target
      fCode         = sortBy (comparing oId) (flatten code)
      b             = code
      o             = fCode
      block         = concatMap
                      (\b -> replicate (length (bCode b)) (bLab b)) code
      ops           = blockMap sort code
      deps          = map DG.dependencies dgs
      dep           = map (map (\(p, c, _) -> (p, c))) deps
      dist          = map (map (\(_, _, ls) -> map modelLatency ls)) deps
      im            = instructionManager fCode
      lat           = map (operationLatency oif im) fCode
      minlive       = map (minLive oif fCode) (sort $ tUniqueOps fCode)
      r2cap         = [(resId ir, resCapacity (res ir)) | ir <- iResources rm]
      cap           = toValueList r2cap
      con           = toValueList $ operationUsages units rm im
      dur           = toValueList $ operationUsages occupation rm im
      off           = toValueList $ operationUsages offset rm im
      factor        = if scaleFreq
                      then scaleFactor (rm, oif, dgs) code else 1.0
      freq          = map (scaleDown factor . blockFreq) code :: [Frequency]
      i             = indexedInstructions im
      r             = iResources rm
      instructions  = map (oIInstructions im) fCode
      activators    = map (activatorIInstructions im) fCode
      maxc          = map (computeMaxC (rm, oif, dgs)) code
      itype         = map ((M.!) typeNumbers . oType) fCode
      insname       = map (show . ioInstruction) i
      part          = map (\o -> (oId o, (aPart (oAs o)))) $ filter (\o -> isJust (aPart (oAs o))) fCode
    in
     [
      -- Program parameters

      -- set of blocks
      ("B", toJSON b),

      -- set of operations
      ("O", toJSON o),

      -- block of each operation
      ("block", toJSON block),

      -- estimation of the execution frequency of each block
      -- example: freq[4]: estimated execution frequency of b4
      ("freq", toJSON freq),

      -- minimum live range duration of each temporary
      -- example: minlive[7]: minimum live range duration of t7
      ("minlive", toJSON minlive),

      -- fixed dependency graph
      -- example: dep[1][3][0]: parent of the third fixed precedence in b1
      --          dep[1][3][1]: child of the third fixed precedence in b1
      ("dep", toJSON dep),

      -- instructions that activate each operation
      -- example: activators[5][2]: third instruction that activates o5
      ("activators", toJSON activators),

      -- Processor parameters

      -- set of instructions
      ("I", toJSON i),

      -- set of processor resources
      ("R", toJSON r),

      -- minimum issue distances of each fixed dependency and parent operation
      -- example: dist[1][3][0]: minimum issue distance in the fourth fixed
      -- precedence of b1, when the parent is implement by its first instruction
      ("dist", toJSON dist),

      -- instructions that can implement each operation
      -- example: instructions[7][2]: third instruction of o7
      ("instructions", toJSON instructions),

      -- latency of each operand when its operation is implemented by each
      --  instruction
      -- example: lat[5][2][3]: latency of p3 when o5 is implemented by its
      -- third instruction
      ("lat", toJSON lat),

      -- capacity of each processor resource
      -- example: cap[1]: capacity of r1
      ("cap", toJSON cap),

      -- consumption of each processor resource by each instruction
      -- example: con[1][3]: consumption of r3 by i1
      ("con", toJSON con),

      -- duration of usage of each processor resource by each instruction
      -- example: dur[4][2]: consumption of r2 by i4
      ("dur", toJSON dur),

      -- offset of usage of each processor resource by each instruction
      -- example: off[2][3]: consumption of r3 by i2
      ("off", toJSON off),


      -- Additional parameters

      -- operations of each block
      -- example: ops[4][1]: second operation of b4
      ("ops", toJSON ops),

      -- maximum issue cycle in each block
      -- example: maxc[5]: maximum issue cycle of b5
      ("maxc", toJSON maxc),

      -- type of each operation
      -- 0:  linear
      -- 1:  branch
      -- 2:  call
      -- 3:  tail call
      -- 4:  in-delimiter
      -- 5:  out-delimiter
      -- 6:  kill
      -- 7:  define
      -- 8:  combine
      -- 9:  low
      -- 10: high
      -- 11: function
      -- 12: copy
      -- example: type[17]: type of o17
      ("type", toJSON itype),

      -- name of each instruction
      -- example: insname[4]: name of i4
      ("insname", toJSON insname),

      -- participative instructions
      -- example: cycle[2][0]: the third participative operation
      --          cycle[2][1]: issue cycle of the third participative operation
      ("part", toJSON part)
     ]

operationLatency oif im o = map (instructionLatency oif o) (oIInstructions im o)

instructionLatency _ o instr
  | (ioInstruction instr == (General NullInstruction)) ||
    (ioInstruction instr == (General VirtualInstruction)) =
      mapLatencies (const nullLatency, const nullLatency) o
instructionLatency _ o instr
  | (ioInstruction instr == (General BarrierInstruction)) =
  mapLatencies (const nullLatency, const barrierLatency) o
instructionLatency oif o
  (IndexedInstruction _ (TargetInstruction instr)) =
    let t2ls = M.fromList $ tempLatencies oif o instr
        p2ls = (M.!) t2ls
    in mapLatencies (p2ls, p2ls) o

nullLatency = 0
barrierLatency = 1

mapLatencies (ulf, dlf) i = map ulf (oUseOperands i) ++ map dlf (oDefOperands i)

minLive oif code t = minLiveOfDef oif t $ potentialDefiner t code

operationUsages f rm im =
  [(ioId io, resourceUsages f rm (ioInstruction io)) | io <- indexedInstructions im]

resourceUsages f rm i =
    let r2u = M.fromList [(r, f u) | IndexedUsage {resourceId = r, usage = u}
                                       <- iUsages rm i]
    in [fromMaybe 0 (M.lookup r r2u) | IndexedResource {resId = r}
                                         <- iResources rm]

modelLatency :: Num a => Maybe a -> a
modelLatency = fromMaybe 0

typeNumbers :: M.Map OperationT Integer
typeNumbers =
    M.fromList $ zip (map NaturalType naturalTypes ++
                      map VirtualType virtualTypes ++ [CopyType])
                      [0..]

naturalTypes = [LinearType, BranchType, CallType, TailCallType]

virtualTypes =
  map DelimiterType delimiterTypes ++
  [KillType, DefineType, CombineType, LowType, HighType, FunType]

delimiterTypes = [InType, OutType]
