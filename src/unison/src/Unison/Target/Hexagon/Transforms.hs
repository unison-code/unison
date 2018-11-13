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
     discardSpills,
     addCSLoadEffects,
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
import Unison.Target.Hexagon.HexagonRegisterDecl

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
-- o1: [p3{t3}] <- {C2_cmpeqi, C2_cmpeqi_combo} [p1{t1, ..}, p2{t2, ..}]

expandJumps _ _ (
  c @ SingleOperation {oOpr = Natural (Linear {
                                          oIs = [TargetInstruction i],
                                          oUs = [_, u2],
                                          oDs = [MOperand {altTemps = [d]}]})}
  :
  os) _ | isCmpInstr i && any (\j -> isJumpOpr j && isPotentialUser d j) os =
  let -- if the compare instruction i can form a new-value compare and jump
      -- (together with its corresponding jump), add a combo-cmp instruction
      ci = cmpComboInstr i
      c1 = if isNewValueJumpCandidateInstr i u2 then
              mapToInstructions (\is -> is ++ [TargetInstruction ci]) c
           else c
  in (os, [c1])

-- Example:
--
-- o1: [] <- J2_jumpf [p1{t1}, b]
-- (where t1 is defined by a cmp instruction 'c' in the block)
-- ->
-- o1: [] <- {J2_jumpf, J2_jumpf_nv, J4_combo_f_jumpnv_t} [p1{t1},b]

expandJumps to f (
  j @ SingleOperation {oOpr = Natural jo @ (Branch {
                         oBranchIs = [TargetInstruction i],
                         oBranchUs = [MOperand {altTemps = ts}, _]})}
  :
  os) _ | isConditionalBranchInstr i =
    let bcode = bCode $ fromJust $ blockOf (fCode f) j
        -- normally we expect |ds| == 1
        ds = [potentialDefiner t bcode | t <- ts,
              not (isCopy $ potentialDefiner t bcode)]
        ejs =
          case ds of
           [SingleOperation {oOpr = Natural (Linear {
                                        oIs = [TargetInstruction ci],
                                        oUs = [_, u2]})}]
             -- A new-value compare and jump can be used
             | isNewValueJumpCandidateInstr ci u2 ->
               let jis = [i, newValueJump i, newValueCmpJump i]
               in [j {oOpr = Natural jo {oBranchIs = map TargetInstruction jis}}]
             -- Still allow the jump to fetch its input in the same cycle. TODO:
             -- this is pessimistic w.r.t. size, study better how to create and
             -- add "J2_cmpeqi_f_jump_t_linear" and the like and add as
             -- alternative instructions to 'cjl' above
             | otherwise ->
               let i' = newValueJump i
                   is = [i'] ++ (if preserveDominatedIns to then [i] else [])
               in [j {oOpr = Natural jo {oBranchIs = map TargetInstruction is}}]
           _ -> [j]
    in (os, ejs)

expandJumps _ _ (o : rest) _ = (rest, [o])

blockOf code o = find (\b -> any (isIdOf o) (bCode b)) code

newValueJump J2_jumpt = J2_jumpt_nv
newValueJump J2_jumpf = J2_jumpf_nv

-- We assume the "taken" until 'addJumpHint' during post-processing
newValueCmpJump J2_jumpt = J4_combo_t_jumpnv_t
newValueCmpJump J2_jumpf = J4_combo_f_jumpnv_t

discardSpills f @ Function {fCode = code} =
  let f1 = mapToOperation (discardSpill (flatten code)) f
      f2 = removeInactiveOperations f1
  in f2

discardSpill code o @ SingleOperation {oOpr = co @ Copy {oCopyIs = is},
                                       oAs = as} =
  case fmap (\roid -> fromJust $ find (isId roid) code) (aRematOrigin as) of
   Just ro ->
     let ris = map originalInstr [i | TargetInstruction i <- oInstructions ro]
         is' = filter (\i -> isNullInstruction i ||
                             (isTargetInstruction i &&
                              isUsefulCopyFor (head ris) (oTargetInstr i))) is
     in o {oOpr = co {oCopyIs = is'}}
   Nothing -> o
discardSpill _ o = o

-- rematerialization copies are always useful
isUsefulCopyFor _ ci | isDematInstr ci || isRematInstr ci = True
-- copies for constant-extended rematerializable instructions are always useful
-- as they only take one slot
isUsefulCopyFor ri _ | isConstantExtended ri = True
-- spill copies are never useful for rematerializable instructions since a
-- single store or load consumes as much as them (rematerializable instructions
-- are either transfer-like (SLOT0123), predicate-transfer-like (SLOT23 but no
-- spilling), or load-like (SLOT01))
isUsefulCopyFor _ ci | ci `elem` spillInstrs = False
-- other copies are useful
isUsefulCopyFor _ _ = True

removeInactiveOperations f @ Function {fCode = code} =
  let os    = filter isInactive (flatten code)
      ts    = concatMap extractTemps $ concatMap oDefOperands os
      code' = filterCode (\o -> not (o `elem` os)) code
      f'    = mapToOperation (mapToModelOperand (delAlts ts)) f {fCode = code'}
  in f'

delAlts dts p @ MOperand {altTemps = ts} = p {altTemps = ts \\ dts}

isInactive o =
  case oInstructions o of
   [i] | isNullInstruction i -> True
   _ -> False

-- Add R29 read effect to callee-saved loads in exit blocks. This is not
-- done in general as the side-effect would be too restrictive, we allow
-- for example spills before stack allocation instructions in entry blocks.

addCSLoadEffects f @ Function {fCode = code} =
  let code' = mapIf isExitBlock addCSLoadEffect code
  in f {fCode = code'}

addCSLoadEffect b @ Block {bCode = code} =
    let code' = mapIf isCalleeSavedLoad
                (mapToReads ((++ [OtherSideEffect R29]))) code
    in b {bCode = code'}

isCalleeSavedLoad o = isCopy o &&
  -- Callee-saved loads are the only copies with these alternative instructions:
  oInstructions o == [General NullInstruction, TargetInstruction LDD]

-- Allocate a region in the stack frame for passing arguments to callees

allocateArgArea f @ Function {fStackArgSize = s,
                              fFixedStackFrame = fobjs, fStackFrame = objs}
  | s > 0 =
    let size   = frameSize (fobjs ++ objs)
        fstIdx = newFrameIndex objs
        objs'  = objs ++ [mkFrameObject fstIdx size (Just s) s Nothing]
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
            objs'  = objs ++ [mkFrameObject fstIdx size (Just r) 1 Nothing]
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
  | isMuxTransferInstr i = muxAlternatives us
      --let
      --let i' = if any isGTSigned8BitsImm us then [] else [i]
      --in i' ++ [condTransferInstr (i, False), condTransferInstr (i, True)]
  | otherwise = [i]

data MuxImmSize = Small | Medium | Large deriving (Show, Eq, Ord)
data MuxOperandType = MuxReg | MuxImm MuxImmSize deriving Show

-- Select mux alternatives depending on their operand types. The C2_mux
-- variants occupy only one slot (except if they are constant-extended) and
-- their result can be new-valued. The C2_mux*_tfr_new variants occupy two
-- slots, and they can pick a new value of their used predicate. The base
-- C2_mux* are never better than their C2_mux counterparts, but of both
-- operands are immediates and at least the second one is large, there is
-- no other option.
muxAlternatives [_, s1, s2] =
  case (muxOperandType s1, muxOperandType s2) of
   -- Reg Reg
   (MuxReg,        MuxReg)        -> [C2_mux,          C2_mux_tfr_new]
   -- Imm Imm
   (MuxImm Small,  MuxImm Small)  -> [C2_muxii,        C2_muxii_tfr_new]
   (MuxImm Medium, MuxImm Small)  -> [C2_muxii_ce,     C2_muxii_tfr_new]
   (MuxImm Large,  MuxImm Small)  -> [C2_muxii_ce,     C2_muxii_tfr_new_ce]
   (MuxImm i1,     MuxImm i2)
     | all ((<=Medium)) [i1, i2]  -> [C2_muxii_tfr,    C2_muxii_tfr_new]
     | otherwise                  -> [C2_muxii_tfr_ce, C2_muxii_tfr_new_ce]
   -- Reg Imm
   (MuxReg,        MuxImm Small)  -> [C2_muxir,        C2_muxir_tfr_new]
   (MuxReg,        MuxImm Medium) -> [C2_muxir_ce,     C2_muxir_tfr_new]
   (MuxReg,        MuxImm Large)  -> [C2_muxir_ce,     C2_muxir_tfr_new_ce]
   -- Imm Reg
   (MuxImm Small,  MuxReg)        -> [C2_muxri,        C2_muxri_tfr_new]
   (MuxImm Medium, MuxReg)        -> [C2_muxri_ce,     C2_muxri_tfr_new]
   (MuxImm Large,  MuxReg)        -> [C2_muxri_ce,     C2_muxri_tfr_new_ce]

muxOperandType Temporary {} = MuxReg
muxOperandType (Bound MachineImm {miValue = im})
  | isSInt 8 im = MuxImm Small
  | isSInt 12im = MuxImm Medium
  | otherwise   = MuxImm Large

isSInt n v =
  let p = 2 ^ (n - 1)
  in v >= (-p) && v < p
