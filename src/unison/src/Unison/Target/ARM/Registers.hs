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
module Unison.Target.ARM.Registers
    (registerArray, registerAtoms, regClasses, registers,
     subRegIndexType, infRegClassUsage, infRegClassBound,
     reserved, callerSaved, calleeSaved) where

import Data.List
import qualified Data.Map as M

import Unison
import Unison.Target.ARM.ARMRegisterDecl
import Unison.Target.ARM.ARMRegisterClassDecl

-- | Register array

registerArray = [RegisterClass GPR, RegisterClass SPR, RegisterClass CCR,
                 RegisterClass F32, InfiniteRegisterClass M32,
                 InfiniteRegisterClass RM32]

-- | Register atoms of 1-width registers

registerAtoms ra
    | ra `elem` concatMap (registers . RegisterClass)
      [GPR, RGPR, GPRnopc, TcGPR, GPRsp, SPR, CCR, TGPR, F32] =
      (ra, ra)

-- | Register atoms of 2-width registers

registerAtoms D0  = (S0, S1)
registerAtoms D1  = (S2, S3)
registerAtoms D2  = (S4, S5)
registerAtoms D3  = (S6, S7)
registerAtoms D4  = (S8, S9)
registerAtoms D5  = (S10, S11)
registerAtoms D6  = (S12, S13)
registerAtoms D7  = (S14, S15)
registerAtoms D8  = (S16, S17)
registerAtoms D9  = (S18, S19)
registerAtoms D10 = (S20, S21)
registerAtoms D11 = (S22, S23)
registerAtoms D12 = (S24, S25)
registerAtoms D13 = (S26, S27)
registerAtoms D14 = (S28, S29)
registerAtoms D15 = (S30, S31)

-- | Register atoms of 4-width registers

registerAtoms R0_3  = (R0, R3)
registerAtoms R4_7  = (R4, R7)
registerAtoms R8_11 = (R8, R11)

-- | Register atoms of 16-width registers

registerAtoms D0_7 = (S0, S15)
registerAtoms D8_15 = (S16, S31)

registerAtoms r = error ("unmatched: registerAtoms " ++ show r)

-- | Register classes
regClasses =
    map RegisterClass
    [GPR, RGPR, GPRnopc, TcGPR, GPRsp, SPR, DPR, CCR, TGPR, ALL, CS, CSL, CSH, CRS, FCS, FCRS, F32] ++
    map InfiniteRegisterClass [M32, M32t, M64, M128, M512, RM32, RM64]

-- | Individual registers of each register class

registers (RegisterClass GPR) =
    [R0, R1, R2, R3, R4, R5, R6, R7, R8,
     R9, R10, R11, R12, SP, LR, PC]

registers (RegisterClass TGPR) =
    [R0, R1, R2, R3, R4, R5, R6, R7]

registers (RegisterClass RGPR) =
  registers (RegisterClass GPR) \\ [PC, SP]

registers (RegisterClass GPRnopc) =
  registers (RegisterClass GPR) \\ [PC]

registers (RegisterClass TcGPR) = [R0, R1, R2, R3, R12]

registers (RegisterClass GPRsp) = [SP]

registers (RegisterClass SPR) =
    [S0, S1, S2, S3, S4, S5, S6, S7,
     S8, S9, S10, S11, S12, S13, S14, S15,
     S16, S17, S18, S19, S20, S21, S22, S23,
     S24, S25, S26, S27, S28, S29, S30, S31]

registers (RegisterClass ALL) =
  registers (RegisterClass GPR) ++ registers (RegisterClass SPR)

registers (RegisterClass DPR) =
    [D0, D1, D2, D3, D4, D5, D6, D7,
     D8, D9, D10, D11, D12, D13, D14, D15]

registers (RegisterClass CS) = [R4_7, R8_11]

registers (RegisterClass CSL) = [R4_7]

registers (RegisterClass CSH) = [R8_11]

registers (RegisterClass CRS) = [R0_3]

registers (RegisterClass CCR) = [CPSR]

registers (RegisterClass FCS) = [D8_15]

registers (RegisterClass FCRS) = [D0_7]

registers (RegisterClass QPR) =
    [Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9 ,Q10, Q11, Q12, Q13, Q14, Q15]

registers (RegisterClass F32) = [F0]

registers rc = error ("unmatched: registers " ++ show rc)

-- | Index type (low/high/copy) of subregisters

subRegIndexType _ subreg = error ("unmatched: subRegIndexType " ++ show subreg)

-- | Map from infinite register class to register usage

infRegClassUsage (InfiniteRegisterClass rc)
  | rc == M32  = 1
  | rc == M32t = 1
  | rc == M64  = 2
  | rc == M128 = 4
  | rc == M512 = 16
  | rc == RM32 = 1
  | rc == RM64 = 2

-- | Map from infinite register class to (possibly) register atom upper bound

infRegClassBound (InfiniteRegisterClass rc)
  | rc == M32t = Just 32
  | otherwise = Nothing

-- | Registers whose value cannot be moved around

reserved = [SP, LR, PC]

-- | Caller- and callee-saved registers

-- | Registers that are not preserved across calls
callerSaved = [R0_3, R12, D0_7]

-- | Registers that are preserved across calls
calleeSaved = [R4_7, R8_11, D8_15]

instance Read ARMRegister where
  readsPrec _ s = [(readReg s, "")]

readReg s = case M.lookup s (inverseMap regStrings) of
              (Just r) -> r
              Nothing -> error $ "unmatched: readReg " ++ s

instance Show ARMRegister where
  show r = case M.lookup r regStrings of
             (Just s) -> s
             Nothing -> error $ "unmatched: show ARMRegister"

regStrings = M.fromList $
  [(R0, "r0"),
   (R1, "r1"),
   (R2, "r2"),
   (R3, "r3"),
   (R4, "r4"),
   (R5, "r5"),
   (R6, "r6"),
   (R7, "r7"),
   (R8, "r8"),
   (R9, "r9"),
   (R10, "r10"),
   (R11, "r11"),
   (R12, "r12"),
   (SP, "sp"),
   (LR, "lr"),
   (PC, "pc")] ++
  regStringsWithIndex "s" SPR ++
  regStringsWithIndex "d" DPR ++
  [(R0_3, "r0_3"),
   (R4_7, "r4_7"),
   (R8_11, "r8_11")] ++
  [(D0_7, "d0_7"),
   (D8_15, "d8_15")] ++
  [(CPSR, "cpsr")] ++
  [(FPSCR_NZCV, "fpscr_nzcv"),
   (ITSTATE, "itstate"),
   (PRED, "pred"),
   (FPSCR, "fpscr")] ++
  regStringsWithIndex "q" QPR ++
  [(F0, "f0")]

regStringsWithIndex pre rc =
  [(r, pre ++ show idx) | (r, idx) <- zip (registers (RegisterClass rc)) [0..]]
