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
     selectNewValueCompareJumps) where

import Data.List
import qualified Data.Map as M

import Unison
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
  rest) (ti, _) | Register ret `elem` outs =
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

selectNewValueCompareJumps f @ Function {fCode = code} =
  f {fCode = map (selectNewValueCompareJumpsInBlock (flatten code)) code}

selectNewValueCompareJumpsInBlock fcode b @ Block {bCode = code} =
  case find isCmpOpr code of
    (Just c) ->
      let t = fromSingleton (oDefs c)
      in case find isConditionalBranch code of
        (Just br) | users t fcode == [br] ->
          let code'  = if isNewValueJumpCandidateOpr c then
                         mapIf (isIdOf c)  (mapToInstructions addNVInstr) code
                       else code
              code'' = mapIf (isIdOf br) (mapToInstructions addNVInstr) code'
          in b {bCode = code''}
        _ -> b
    Nothing -> b

addNVInstr [TargetInstruction i] = map TargetInstruction [toNVInstr i, i]

toNVInstr i = read (show i ++ "_nv")

isCmpOpr = isSingleLinearInstrOf isCmpInstr

isNewValueJumpCandidateOpr = isSingleLinearInstrOf isNewValueJumpCandidateInstr

isSingleLinearInstrOf p SingleOperation {
  oOpr = Natural Linear {oIs = [TargetInstruction i]}} = p i
isSingleLinearInstrOf _ _ = False

isConditionalBranch SingleOperation {
  oOpr = Natural Branch {oBranchIs = [TargetInstruction i]}} =
  isConditionalBranchInstr i
isConditionalBranch _ = False

-- See `isNewValueJumpCandidate` in HexagonNewValueJump.cpp
isNewValueJumpCandidateInstr i =
  i `elem` [C2_cmpeq, C2_cmpeqi, C2_cmpgt, C2_cmpgti, C2_cmpgtu, C2_cmpgtui,
            C4_cmpneq, C4_cmplte, C4_cmplteu]

isConditionalBranchInstr i = i `elem` [J2_jumpt, J2_jumpf]

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

-- | New-value pairs of conditional transfers

-- TODO: look at HexagonExpandCondsets.cpp

alternativeInstructions _ = []
