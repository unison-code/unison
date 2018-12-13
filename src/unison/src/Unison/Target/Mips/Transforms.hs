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
module Unison.Target.Mips.Transforms
    (rs2ts,
     normalizeCallPrologue,
     normalizeCallEpilogue,
     extractReturnRegs,
     hideStackPointer,
     clobberRAInCall,
     insertGPDisp,
     markBarriers,
     enforceMandatoryFrame,
     cleanClobbers) where

import Unison
import MachineIR
import Unison.Target.Mips.Common
import Unison.Target.Mips.MipsRegisterDecl
import Unison.Target.Mips.SpecsGen.MipsInstructionDecl

-- | Gives patterns as sequences of instructions and replacements where
-- | registers are transformed into temporaries


rs2ts _ (
  m @ SingleOperation {
          oOpr = (Natural mi @ Linear {
                                   oIs = [TargetInstruction mName],
                                   oDs  = [Register rlo, Register rhi]})}
  :
  mflo @ SingleOperation {
          oOpr = (Natural mfloi @ Linear {oIs = [TargetInstruction MFLO]})}
  :
  mfhi @ SingleOperation {
          oOpr = (Natural mfhii @ Linear {oIs = [TargetInstruction MFHI]})}
  :
  rest) (ti, _, _) | mName `elem` [MULT, MULTu] =
  let tlo = mkPreAssignedTemp ti (Register rlo)
      thi = mkPreAssignedTemp (ti + 1) (Register rhi)
  in
   (
     rest,
     [m    {oOpr = Natural mi {oDs = [tlo, thi]}},
      mflo {oOpr = Natural mfloi {oUs = [tlo]}},
      mfhi {oOpr = Natural mfhii {oUs = [thi]}}]
   )

rs2ts _ (
  m @ SingleOperation {
        oOpr = (Natural mi @ (Linear {
                                 oIs = [TargetInstruction mName],
                                 oDs  = [Register rlo, Register rhi]}))}
  :
  mfhi @ SingleOperation {
        oOpr = (Natural mfhii @ Linear {oIs = [TargetInstruction MFHI]})}
  :
  rest) (ti, _, _) | mName `elem` [MULT, MULTu] =
  let tlo = mkPreAssignedTemp ti (Register rlo)
      thi = mkPreAssignedTemp (ti + 1) (Register rhi)
  in
   (
     rest,
     [m    {oOpr = Natural mi {oDs = [tlo, thi]}},
      mfhi {oOpr = Natural mfhii {oUs = [thi]}}]
   )

rs2ts _ (
  m @ SingleOperation {
        oOpr = Natural mi @ (Linear {
                                oIs = [TargetInstruction mName],
                                oDs  = [Register rlo, Register rhi]})}
  :
  mflo @ SingleOperation {
        oOpr = Natural mfloi @ (Linear {oIs = [TargetInstruction MFLO]})}
  :
  rest) (ti, _, _) | mName `elem` [MULT, MULTu] =
  let tlo = mkPreAssignedTemp ti (Register rlo)
      thi = mkPreAssignedTemp (ti + 1) (Register rhi)
  in
   (
     rest,
     [m    {oOpr = Natural mi {oDs = [tlo, thi]}},
      mflo {oOpr = Natural mfloi {oUs = [tlo]}}]
   )

rs2ts _ (
  d @ SingleOperation {
        oOpr = Natural di @ (Linear {
                                oIs = [TargetInstruction DIV],
                                oDs  = [Register rlo, Register rhi]})}
  :
  mflo @ SingleOperation {
        oOpr = Natural mfloi @ (Linear {oIs = [TargetInstruction MFLO]})}
  :
  rest) (ti, _, _) =
  let tlo = mkPreAssignedTemp ti (Register rlo)
      thi = mkPreAssignedTemp (ti + 1) (Register rhi)
  in
   (
     rest,
     [d    {oOpr = Natural di {oDs = [tlo, thi]}},
      mflo {oOpr = Natural mfloi {oUs = [tlo]}}]
   )


rs2ts _ (
  SingleOperation {
    oId = ucloId, oOpr = Virtual (VirtualCopy {oVirtualCopyS = tUclo,
                                                oVirtualCopyD = Register rlo})}
  :
  SingleOperation {
    oId = uchoId, oOpr = Virtual (VirtualCopy {oVirtualCopyS = tUchi,
                                                oVirtualCopyD = Register rhi})}
  :
  madd @ SingleOperation {
           oOpr = Natural mi @ (Linear {oIs = [TargetInstruction MADD],
                                         oUs  = [_, _, u1, u2]})}
  :
  SingleOperation {
    oId = dcloId, oOpr = Virtual (VirtualCopy {oVirtualCopyD = tDclo})}
  :
  SingleOperation {
    oId = dchoId, oOpr = Virtual (VirtualCopy {oVirtualCopyD = tDchi})}
  :
  rest) (ti, _, _) =
  let tlo  = mkPreAssignedTemp ti (Register rlo)
      thi  = mkPreAssignedTemp (ti + 1) (Register rhi)
      tlo' = mkPreAssignedTemp (ti + 2) (Register rlo)
      thi' = mkPreAssignedTemp (ti + 3) (Register rhi)
  in
   (
     rest,
     [mkLinear ucloId [TargetInstruction MTLO] [tUclo] [tlo],
      mkLinear uchoId [TargetInstruction MTHI] [tUchi] [thi],
      madd {oOpr = Natural mi {oUs = [tlo, thi, u1, u2], oDs = [tlo', thi']}},
      mkLinear dcloId [TargetInstruction MFLO] [tlo'] [tDclo],
      mkLinear dchoId [TargetInstruction MFHI] [thi'] [tDchi]
     ]
   )

rs2ts _ (o : rest) _ = (rest, [o])

-- | Matches call prologues and pre-assigns a temp to the return address

normalizeCallPrologue _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register r})}
  :
  ca1 : ca2 : ca3 : ca4 : ca5
  :
  j @ SingleOperation {oOpr = Natural ji @ (Call {oCallUs = [Register r']})}
  :
  rest) (ti, _, _) | r == r' && all isVirtualCopy [ca1, ca2, ca3, ca4, ca5] =
  let t = mkTemp ti
  in
    (rest,
     [c {oOpr = Virtual ci {oVirtualCopyD = t}},
      ca1, ca2, ca3, ca4, ca5,
      j {oOpr = Natural ji {oCallUs = [preAssign t (Register r)]}}]
    )

normalizeCallPrologue _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register r})}
  :
  ca1 : ca2 : ca3 : ca4
  :
  j @ SingleOperation {oOpr = Natural ji @ (Call {oCallUs = [Register r']})}
  :
  rest) (ti, _, _) | r == r' && all isVirtualCopy [ca1, ca2, ca3, ca4] =
  let t = mkTemp ti
  in
    (rest,
     [c {oOpr = Virtual ci {oVirtualCopyD = t}},
      ca1, ca2, ca3, ca4,
      j {oOpr = Natural ji {oCallUs = [preAssign t (Register r)]}}]
    )

normalizeCallPrologue _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register r})}
  :
  ca1 : ca2 : ca3
  :
  j @ SingleOperation {oOpr = Natural ji @ (Call {oCallUs = [Register r']})}
  :
  rest) (ti, _, _) | r == r' && all isVirtualCopy [ca1, ca2, ca3] =
  let t = mkTemp ti
  in
    (rest,
     [c {oOpr = Virtual ci {oVirtualCopyD = t}},
      ca1, ca2, ca3,
      j {oOpr = Natural ji {oCallUs = [preAssign t (Register r)]}}]
    )

normalizeCallPrologue _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register r})}
  :
  ca1 : ca2
  :
  j @ SingleOperation {oOpr = Natural ji @ (Call {oCallUs = [Register r']})}
  :
  rest) (ti, _, _) | r == r' && all isVirtualCopy [ca1, ca2] =
  let t = mkTemp ti
  in
    (rest,
     [c {oOpr = Virtual ci {oVirtualCopyD = t}},
      ca1, ca2,
      j {oOpr = Natural ji {oCallUs = [preAssign t (Register r)]}}]
    )

normalizeCallPrologue _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register r})}
  :
  ca
  :
  j @ SingleOperation {oOpr = Natural ji @ (Call {oCallUs = [Register r']})}
  :
  rest) (ti, _, _) | r == r' && isVirtualCopy ca =
  let t = mkTemp ti
  in
    (rest,
     [c {oOpr = Virtual ci {oVirtualCopyD = t}},
      ca,
      j {oOpr = Natural ji {oCallUs = [preAssign t (Register r)]}}]
    )

normalizeCallPrologue _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register r})}
  :
  j @ SingleOperation {oOpr = Natural ji @ (Call {oCallUs = [Register r']})}
  :
  rest) (ti, _, _) | r == r' =
  let t = mkTemp ti
  in
    (rest,
     [c {oOpr = Virtual ci {oVirtualCopyD = t}},
      j {oOpr = Natural ji {oCallUs = [preAssign t (Register r)]}}]
    )

normalizeCallPrologue _ (o : rest) _ = (rest, [o])

-- | Matches call epilogues

normalizeCallEpilogue _ (
  lw @ SingleOperation {
          oOpr = Natural (Linear {oIs = [TargetInstruction LW],
                                   oUs  = [Bound MachineImm {},
                                           Register (TargetRegister SP)],
                                   oDs  = []})}
  :
  c1 @ SingleOperation {
        oOpr = Virtual VirtualCopy {oVirtualCopyS = Register _,
                                    oVirtualCopyD = t}}
  :
  c2 @ SingleOperation {
        oOpr = Virtual VirtualCopy {oVirtualCopyS = Register _,
                                    oVirtualCopyD = t'}}
  :
  rest) _ | all isTemporary [t, t'] = (rest, [c1, c2, lw])

normalizeCallEpilogue _ (
  lw @ SingleOperation {
          oOpr = Natural (Linear {oIs = [TargetInstruction LW],
                                   oUs  = [Bound MachineImm {},
                                           Register (TargetRegister SP)],
                                   oDs  = []})}
  :
  c @ SingleOperation {
        oOpr = Virtual VirtualCopy {oVirtualCopyS = Register _,
                                     oVirtualCopyD = t}}
  :
  rest) _ | isTemporary t = (rest, [c, lw])

normalizeCallEpilogue _ (o : rest) _ = (rest, [o])

{-
    o41: [V0] <- (copy) [t5]
    o42: [] <- PseudoReturn [RA]
    o49: [] <- (out) [V0]

    ->

    o41: [t'] <- (copy) [t5]
    o42: [] <- PseudoReturn [RA]
    o49: [] <- (out) [t':V0]
    -}

extractReturnRegs _ (
  c @ SingleOperation {oOpr = Virtual (ci @ VirtualCopy {
                                                oVirtualCopyD = Register ret})}
  :
  r @ SingleOperation {oOpr = Natural Branch {
                          oBranchIs = [TargetInstruction PseudoReturn]}}
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter oi @ (Out {oOuts = [Register ret']}))}
  :
  rest) (ti, _, _) | ret == ret' =
  let t = mkTemp ti
  in
   (
    rest,
    [c {oOpr = Virtual ci {oVirtualCopyD = t}},
     r,
     o {oOpr = Virtual (Delimiter oi {
                            oOuts = [preAssign t (Register ret)]})}]
   )

extractReturnRegs _ (o : rest) _ = (rest, [o])

hideStackPointer o @ SingleOperation {
  oOpr = Natural no @ Linear {oIs = [TargetInstruction i], oUs = us}}
  | any isStackPointer us =
    let i'  = hiddenStackPointerInstruction i
        us' = filter (not . isStackPointer) us
    in o {oOpr = Natural (no {oIs = [TargetInstruction i'], oUs = us'})}
hideStackPointer o = o

isStackPointer = isTargetReg SP

{-
    Redefine $ra by an operation to be scheduled one cycle before function
    calls, to model that the old $ra value is not accesible to delay slot
    instructions:

    ... <- JALRPseudo ...
    ... <- (fun) [...]

    ->

    [t:ra] <- CLOBBER_RA []
    ... <- JALRPseudo ...
    ... <- (fun) [..., t:ra]
-}

clobberRAInCall _ (
  c @ SingleOperation {oOpr = Natural Call {}}
  :
  f @ SingleOperation {oOpr = Virtual fi @ (Fun {oFunctionUs = us})}
  :
  rest) (tid, oid, _) =
  let t = mkPreAssignedTemp tid (Register (TargetRegister RA))
  in
    (rest,
     [
      mkLinear oid [TargetInstruction CLOBBER_RA] [] [t],
      c,
      f {oOpr = Virtual fi {oFunctionUs = us ++ [t]}}]
    )

clobberRAInCall _ (o : rest) _ = (rest, [o])

insertGPDisp _ (
  e @ SingleOperation {oOpr = Virtual (Delimiter (In {oIns = ins}))}
  :
  rest) (_, oid, _)
  | all (\r -> any (isPreAssignedTo r) ins) [T9, V0] =
    let lgp = mkLinear oid [TargetInstruction LoadGPDisp] [] []
    in (rest, [e, lgp])

insertGPDisp _ (o : rest) _ = (rest, [o])

isPreAssignedTo r p =
  case preAssignment p of
   (Just r') -> isTargetReg r r'
   Nothing -> False

isTargetReg r (Register (TargetRegister r')) = r == r'
isTargetReg _ _ = False

markBarriers o @ SingleOperation {
    oOpr = Natural (Linear {oIs = [TargetInstruction i]}), oAs = as}
  | isBarrierInstr i = o {oAs = as {aReads = [], aWrites = [ControlSideEffect]}}
markBarriers o = o

{-
  Mark stack handling instructions mandatory for functions that include calls
  and do not include exit blocks (as at least the return address needs to be
  spilled into the stack).
-}

enforceMandatoryFrame f (
  a @ SingleOperation {
     oOpr = (Natural ai @ Linear {oIs = [_, TargetInstruction i]})}
  :
  rest) _ | i `elem` [ADDiu_sp, ADDiu_negsp] &&
            any isCall (flatCode f) &&
            any isReturnBlock (fCode f) &&
            none isExitBlock (fCode f) =
            let a1 = a {oOpr = Natural ai {oIs = [TargetInstruction i]}}
                a2 = mapToActivators (const []) a1
            in (rest, [a2])

enforceMandatoryFrame _ (o : rest) _ = (rest, [o])

cleanClobbers f = mapToOperation cleanClobber f

cleanClobber o @ SingleOperation {}
  | isClobberRA o = cleanClobber (mkBundle [o])
  | otherwise = o
cleanClobber o @ Bundle {bundleOs = os} =
  o {bundleOs = filter (not . isClobberRA) os}
