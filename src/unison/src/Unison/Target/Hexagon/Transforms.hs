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
module Unison.Target.Hexagon.Transforms
    (extractReturnRegs,
     foldStackPointerCopy,
     addAlternativeInstructions,
     expandJumps,
     addControlBarrier,
     alignAllocFrame,
     shiftFrameOffsets) where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Word

import MachineIR
import Unison
import Unison.Analysis.FrameOffsets
import Unison.Target.Hexagon.Common
import Unison.Target.Hexagon.Registers
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl

extractReturnRegs _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register ret})}
  :
  j @ SingleOperation {oOpr = Natural Branch {
                            oBranchIs = [TargetInstruction JMPret]}}
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter Out {oOuts = outs})}
  :
  rest) (ti, _, _) | Register ret `elem` outs =
  let t = mkTemp ti
      m = M.fromList [(Register ret, preAssign t (Register ret))]
  in
   (
    rest,
    [c {oOpr = Virtual ci {oVirtualCopyD = t}},
     j,
     mapToOperandIf isRegister (applyMap m) o]
   )

extractReturnRegs _ (
  c @ SingleOperation {oOpr = Natural Call {
                            oCallIs = [TargetInstruction J2_callr]}}
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter oi @ (Out {oOuts = outs}))}
  :
  rest) _ | all isRegister outs =
   (
    rest,
    [c,
     o {oOpr = Virtual (Delimiter oi {oOuts = []})}]
   )

extractReturnRegs _ (inst : rest) _ = (rest, [inst])

foldStackPointerCopy _ (
  SingleOperation {oOpr = Virtual (VirtualCopy {
                                         oVirtualCopyS = Register sp,
                                         oVirtualCopyD = t})}
  :
  is) _ | sp == mkTargetRegister hexagonSP =
  let (users, rest) = span (not . isCopyUsing (Register sp)) is
      m             = M.fromList [(t, Register sp)]
      f             = mapToOperandIf isTemporary (applyMap m)
  in
   (
    rest,
    map f users
   )

foldStackPointerCopy _ (inst : rest) _ = (rest, [inst])

isCopyUsing r c = isVirtualCopy c && r `elem` oUses c

addAlternativeInstructions
  o @ SingleOperation {
          oOpr = Natural ao @ (Linear {oIs = [TargetInstruction i]})} =
  let newIs = map TargetInstruction (alternativeInstructions i)
  in o {oOpr = Natural (ao {oIs = nub (TargetInstruction i : newIs)})}
addAlternativeInstructions o = o

-- See `isNewValueJumpCandidate` and `canCompareBeNewValueJump` in
-- HexagonNewValueJump.cpp
isNewValueJumpCandidateInstr i _
  | i `elem` [C2_cmpeq, C2_cmpgt, C2_cmpgtu, C4_cmpneq, C4_cmplte,
              C4_cmplteu] =True
isNewValueJumpCandidateInstr i (Bound MachineImm {miValue = v})
  | i `elem` [C2_cmpeqi, C2_cmpgti, C2_cmpgtui] && isUInt 5 v = True
  | i `elem` [C2_cmpeqi, C2_cmpgti] && v == -1 = True
isNewValueJumpCandidateInstr _ _ = False

isUInt n v = (fromInteger v :: Word64) < (2 ^ n)

isConditionalBranchInstr i = i `elem` [J2_jumpt, J2_jumpf]

isJumpOpr o =
  case oInstructions o of
   [TargetInstruction i] | isJmpInstr i -> True
   _ -> False

-- Example:
--
-- o1: [p3{t3}] <- C2_cmpeqi [p1{t1, ..},p2{t2, ..}]
-- (where t3 is used by a J2_jumpt/J2_jumpf instruction in the block)
-- ->
-- o1: [p3{-, t3}] <- {-, C2_cmpeqi} [p1{-, t1, ..}, p2{-, t2, ..}]

expandJumps _ (
  c @ SingleOperation {oOpr = Natural (Linear {
                                          oIs = [TargetInstruction i],
                                          oDs = [MOperand {altTemps = [d]}]})}
  :
  os) _ | isCmpInstr i && any (\j -> isJumpOpr j && isPotentialUser d j) os =
  let c' = makeOptional c
  in (os, [c'])

-- Example:
--
-- o1: [] <- J2_jumpf [p1{t1}, b]
-- (where t1 is defined by a cmp instruction 'c' in the block)
-- ->
-- o1: [p2{-, t2}] <- {-, J2_jumpf_linear} [p1{-, t1}]
-- o2: [p5{-, t3}] <- {-, J4_cmpeqi_f_jumpnv_t_linear} [p3{-, t3}, p4{-, t4}]
-- o3: [] <- jump_merge [p34{t2, t3},b]
-- (where p3, p4 are renumbered copies of the operands of 'c')

expandJumps f (
  j @ SingleOperation {oOpr = Natural jo @ (Branch {
                         oBranchIs = [TargetInstruction i],
                         oBranchUs = [p1 @ MOperand {altTemps = [t1]}, l]}),
                       oAs = as}
  :
  os) (tid, oid, pid)
  | isConditionalBranchInstr i =
    let b   = fromJust $ blockOf (fCode f) j
        ejs =
          case find (isPotentialDefiner t1) (bCode b) of
           Just (c @ SingleOperation {oOpr = Natural (Linear {
                                        oIs = [TargetInstruction ci],
                                        oUs = [_, u2]})})
             -- A new-value compare and jump can be used
             | isNewValueJumpCandidateInstr ci u2 ->
               let jis = linearJumps i
                   jl  = mkLinear oid (map TargetInstruction jis) [p1]
                         [mkMOp pid [mkTemp tid]]
                   cji = linearNewValueCmpJump ci i
                   cjl = mkLinear (oid + 1) [TargetInstruction cji]
                         (map updateMOperandId (zip [pid + 1 ..] (oUses c)))
                         [mkMOp (pid + 3) [mkTemp tid + 1]]
                   jm  = (mkBranch (oid + 2) [TargetInstruction Jump_merge]
                          [mkMOp (pid + 4) (map mkTemp [tid, tid + 1]), l])
                         {oAs = as}
               in map makeOptional [jl, cjl] ++ [jm]
             -- Still allow the jump to fetch its input in the same cycle. TODO:
             -- this is pessimistic w.r.t. size, study better how to create and
             -- add "J2_cmpeqi_f_jump_t_linear" and the like and add as
             -- alternative instructions to 'cjl' above
             | otherwise ->
               let i' = newValueJump i
               in [j {oOpr = Natural jo {oBranchIs = [TargetInstruction i']}}]
           _ -> [j]
    in (os, ejs)

expandJumps _ (o : rest) _ = (rest, [o])

blockOf code o = find (\b -> any (isIdOf o) (bCode b)) code

updateMOperandId (pid, p @ MOperand {}) = p {operandId = pid}
updateMOperandId (_, p) = p

linearJumps J2_jumpt = [J2_jumpt_linear, J2_jumpt_nv_linear]
linearJumps J2_jumpf = [J2_jumpf_linear, J2_jumpf_nv_linear]

newValueJump J2_jumpt = J2_jumpt_nv
newValueJump J2_jumpf = J2_jumpf_nv

-- We assume the "taken" hint all the time
linearNewValueCmpJump cmp jmp =
  read $
  "J4_" ++
  (normalForm $ dropPrefix "C2_" $ show cmp) ++ "_" ++
  (if isTrueJump jmp then "t" else "f") ++
  "_jumpnv_t_linear"

normalForm "cmplteu" = "cmpltu"
normalForm i = i

isTrueJump J2_jumpt = True
isTrueJump J2_jumpf = False

mkMOp id ts = mkMOperand id ts Nothing

addControlBarrier o @ SingleOperation {oOpr = Natural Linear {oIs = is},
                                       oAs = as} =
  let is' = [i | TargetInstruction i <- is]
  in if any (\i -> isLinearJump i || isLinearNewValueCmpJump i) is' then
       o {oAs = as {aReads = [], aWrites = [ControlSideEffect]}}
     else o
addControlBarrier o = o

-- Introduce "slack" frame object to align the offset of 'allocframe' to 8 bytes

alignAllocFrame f @ Function {fFixedStackFrame = fobjs,
                              fStackFrame = objs} =
  let size  = frameSize (fobjs ++ objs)
      r     = size `rem` 8
  in case r of
      0 -> f
      _ ->
        let fstIdx = newFrameIndex objs
            objs'  = objs ++ [mkFrameObject fstIdx size (Just r) 4]
        in f {fFixedStackFrame = fobjs, fStackFrame = objs'}

-- Offset frame indices before (-8) and after (+d) 'allocframe'

shiftFrameOffsets f @ Function {fCode = code,
                                fFixedStackFrame = fobjs,
                                fStackFrame = objs} =
  let d     = maximum $ (map (abs . foOffset) (fobjs ++ objs)) ++ [0]
      code' = map (shiftFrameOffsetsInBlock d) code
  in f {fCode = code'}

shiftFrameOffsetsInBlock d b @ Block {bCode = code} =
  let ini = if isEntryBlock b then -8 else d
      (_, code') = mapAccumL (shiftFrameOffsetsInOpr d) ini code
  in b {bCode = code'}

shiftFrameOffsetsInOpr d off o =
  let o'   = mapToOperandIf always (shiftFrameOffset off) o
      off' = if any isAllocFrameOpr (linearizeOpr o) then d else off
  in (off', o')

shiftFrameOffset off (Bound mfi @ (MachineFrameIndex {})) =
  Bound $ mfi {mfiOffset = off}
shiftFrameOffset _ p = p

isAllocFrameOpr o =
  isNatural o && targetInst (oInstructions o) == S2_allocframe

-- | Gives alternative instructions with the same semantics

-- | New-value stores

alternativeInstructions S2_storerb_io = [S2_storerbnew_io]
alternativeInstructions S2_storerb_pi = [S2_storerbnew_pi]
alternativeInstructions S2_storerh_io = [S2_storerhnew_io]
alternativeInstructions S2_storerh_pi = [S2_storerhnew_pi]
alternativeInstructions S2_storeriabs = [S2_storerinewabs]
alternativeInstructions S2_storeri_io = [S2_storerinew_io]
alternativeInstructions S2_storeri_io_fi = [S2_storerinew_io_fi]
alternativeInstructions S2_storeri_pi = [S2_storerinew_pi]
alternativeInstructions S4_storerh_rr = [S4_storerhnew_rr]
alternativeInstructions S4_storeri_rr = [S4_storerinew_rr]
alternativeInstructions S2_storerb_io_ce = [S2_storerbnew_io_ce]
alternativeInstructions S2_storerb_pi_ce = [S2_storerbnew_pi_ce]
alternativeInstructions S2_storerh_io_ce = [S2_storerhnew_io_ce]
alternativeInstructions S2_storerh_pi_ce = [S2_storerhnew_pi_ce]
alternativeInstructions S2_storeriabs_ce = [S2_storerinewabs_ce]
alternativeInstructions S2_storeri_io_ce = [S2_storerinew_io_ce]
alternativeInstructions S2_storeri_io_fi_ce = [S2_storerinew_io_fi_ce]
alternativeInstructions S2_storeri_pi_ce = [S2_storerinew_pi_ce]
alternativeInstructions S4_storerh_rr_ce = [S4_storerhnew_rr_ce]
alternativeInstructions S4_storeri_rr_ce = [S4_storerinew_rr_ce]

-- | New-value pairs of conditional transfers

-- TODO: look at HexagonExpandCondsets.cpp

alternativeInstructions _ = []
