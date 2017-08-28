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
module Unison.Target.Hexagon.Transforms
    (liftStackArgSize,
     extractReturnRegs,
     foldStackPointerCopy,
     addAlternativeInstructions,
     expandJumps,
     addControlBarrier,
     allocateArgArea,
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

liftStackArgSize f @ Function {fCode = code} =
  let fcode = flatten code
      sizes = [s | Bound (MachineImm {miValue = s}) <-
                  [oSingleUse o | o <- fcode, isFrameSetup o]]
      size  = maybeMax 0 sizes
  in f {fStackArgSize = size}

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
  c @ SingleOperation {oOpr = Natural nc}
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter oi @ (Out {oOuts = outs}))}
  :
  rest) _ | all isRegister outs &&
            ((isCall c && oCallIs nc == [TargetInstruction J2_callr]) ||
             isTailCall c) =
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
          oOpr = Natural n @ (Linear {oIs = [TargetInstruction i], oUs = us})} =
  let is = map TargetInstruction (alternativeInstructions i us)
  in o {oOpr = Natural (n {oIs = nub is})}
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

-- Allocate a region in the stack frame for passing arguments to callees

allocateArgArea f @ Function {fStackArgSize = s,
                              fFixedStackFrame = fobjs, fStackFrame = objs}
  | s > 0 =
    let size   = frameSize (fobjs ++ objs)
        fstIdx = newFrameIndex objs
        objs'  = objs ++ [mkFrameObject fstIdx size (Just s) s]
    in f {fStackArgSize = 0, fFixedStackFrame = fobjs, fStackFrame = objs'}
  | otherwise = f

-- Introduce "slack" frame object to align the offset of 'allocframe' to 8
-- bytes. TODO: introduce this frame object in the same stack frame region
-- as LLVM for consistency.

alignAllocFrame f @ Function {fFixedStackFrame = fobjs,
                              fStackFrame = objs} =
  let size  = frameSize (fobjs ++ objs)
      r     = case size `rem` 8 of
               s -> 8 - s
  in case r of
      8 -> f
      _ ->
        let fstIdx = newFrameIndex objs
            objs'  = objs ++ [mkFrameObject fstIdx size (Just r) 1]
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

alternativeInstructions i us
  | isOldValueStoreInstr i = [i, newValueStoreInstr i]
  -- see HexagonExpandCondsets.cpp. TODO: handle MUX64_rr
  | isMuxTransferInstr i =
      let i' = if any isGTSigned8BitsImm us then [] else [i]
      in i' ++ [condTransferInstr (i, False), condTransferInstr (i, True)]
  | otherwise = [i]

isGTSigned8BitsImm (Bound MachineImm {miValue = imm})
  | not (isSInt 8 imm) = True
isGTSigned8BitsImm _ = False

isSInt n v =
  let v' = fromInteger v :: Word64
      p  = 2 ^ (n - 1)
  in v' >= (-p) && v' < p
