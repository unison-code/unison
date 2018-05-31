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
module Unison.Target.Mips.Registers
    (registerArray, registerAtoms, regClasses, registers,
     subRegIndexType, infRegClassUsage, infRegClassBound,
     reserved, callerSaved, calleeSaved) where

import qualified Data.Map as M

import Unison

import Unison.Target.Mips.MipsRegisterDecl
import Unison.Target.Mips.MipsRegisterClassDecl

-- | Register array configuration

-- | Register array

registerArray =
    map RegisterClass
    [GPR32Opnd, ACC64, FGR32Opnd, FCCRegsOpnd, DSPCC] ++
    map InfiniteRegisterClass [M32, RM32]

-- | Register classes
regClasses =
  map RegisterClass
  [GPR32Opnd, GPR32, GPR_ATT7, GPR_T89, GPR_FPRA, ACC64, FGR32Opnd, FGR32,
   AFGR64Opnd, AFGR64, AFGR_D09, FCCRegsOpnd, DSPCC] ++
  map InfiniteRegisterClass [M32, M64, RM32, RM64]

-- | Register atoms of 1-width registers

registerAtoms ra
    | ra `elem` concatMap (registers . RegisterClass)
      [GPR32Opnd, GPR32, ACC64, FGR32Opnd, FGR32, FCCRegsOpnd, DSPCC] =
      (ra, ra)

-- | Register atoms of 2-width registers

registerAtoms D0  = (F0, F1)
registerAtoms D1  = (F2, F3)
registerAtoms D2  = (F4, F5)
registerAtoms D3  = (F6, F7)
registerAtoms D4  = (F8, F9)
registerAtoms D5  = (F10, F11)
registerAtoms D6  = (F12, F13)
registerAtoms D7  = (F14, F15)
registerAtoms D8  = (F16, F17)
registerAtoms D9  = (F18, F19)
registerAtoms D10 = (F20, F21)
registerAtoms D11 = (F22, F23)
registerAtoms D12 = (F24, F25)
registerAtoms D13 = (F26, F27)
registerAtoms D14 = (F28, F29)
registerAtoms D15 = (F30, F31)

-- | Register atoms of wider registers

registerAtoms ATT7 = (AT, T7)
registerAtoms T89  = (T8, T9)
registerAtoms FPRA = (FP, RA)
registerAtoms D09  = (F0, F19)

-- | Individual registers of each register class

registers (RegisterClass rc)
  | rc `elem` [GPR32Opnd, GPR32] =
    [ZERO, AT, V0, V1,
     A0, A1, A2, A3,
     T0, T1, T2, T3, T4, T5, T6, T7,
     S0, S1, S2, S3, S4, S5, S6, S7,
     T8, T9,
     K0, K1,
     GP, SP, FP, RA]
  | rc `elem` [GPR_ATT7] = [ATT7]
  | rc `elem` [GPR_T89] = [T89]
  | rc `elem` [GPR_FPRA] = [FPRA]
  | rc `elem` [ACC64] = [AC0]
  | rc `elem` [FGR32Opnd, FGR32] =
      [F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15,
       F16, F17, F18, F19, F20, F21, F22, F23, F24, F25, F26, F27, F28, F29,
       F30, F31]
  | rc `elem` [AFGR64Opnd, AFGR64] =
      [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15]
  | rc `elem` [AFGR_D09] = [D09]
 -- Only the first register since the class is reserved and FCC0 is the
 -- only register that appears in the input MIR:
  | rc `elem` [FCCRegsOpnd] = [FCC0]
  | rc `elem` [DSPCC] = [DSPCCond]

-- | Map from infinite register class to register usage

infRegClassUsage (InfiniteRegisterClass M32)  = 1
infRegClassUsage (InfiniteRegisterClass M64)  = 2
infRegClassUsage (InfiniteRegisterClass RM32) = 1
infRegClassUsage (InfiniteRegisterClass RM64) = 2

-- | Map from infinite register class to (possibly) register atom upper bound

infRegClassBound = const Nothing

-- | Registers whose value cannot be moved around

reserved = [ZERO, K0, K1, SP]

-- | Index type (low/high/copy) of subregisters

subRegIndexType _ subreg = error ("unmatched: subRegIndexType " ++ show subreg)

-- | Caller- and callee-saved registers (extracted from "SYSTEM V APPLICATION
-- BINARY INTERFACE MIPS RISC Processor Supplement", 3rd Edition, 1996)

-- | Registers that are not preserved across calls
-- FIXME: check out the specifics of FP
callerSaved = [ATT7, T89, GP, FPRA, AC0, D09]

-- | Registers that are preserved across calls
calleeSaved = [S0, S1, S2, S3, S4, S5, S6, S7,
               D10, D11, D12, D13, D14, D15]

instance Read MipsRegister where
  readsPrec _ s = [(readReg s, "")]

readReg s = case M.lookup s (inverseMap regStrings) of
              (Just r) -> r
              Nothing -> error $ "unmatched: readReg " ++ s

instance Show MipsRegister where
  show r = case M.lookup r regStrings of
             (Just s) -> s
             Nothing -> error $ "unmatched: show MipsRegister"

regStrings = M.fromList $
  [(ZERO, "zero"),
   (AT, "at"),
   (V0, "v0"),
   (V1, "v1"),
   (A0, "a0"),
   (A1, "a1"),
   (A2, "a2"),
   (A3, "a3"),
   (T0, "t0"),
   (T1, "t1"),
   (T2, "t2"),
   (T3, "t3"),
   (T4, "t4"),
   (T5, "t5"),
   (T6, "t6"),
   (T7, "t7"),
   (S0, "s0"),
   (S1, "s1"),
   (S2, "s2"),
   (S3, "s3"),
   (S4, "s4"),
   (S5, "s5"),
   (S6, "s6"),
   (S7, "s7"),
   (T8, "t8"),
   (T9, "t9"),
   (K0, "k0"),
   (K1, "k1"),
   (GP, "gp"),
   (SP, "sp"),
   (FP, "fp"),
   (RA, "ra")] ++
  [(ATT7, "att7")] ++
  [(T89, "t89")] ++
  [(FPRA, "fpra")] ++
  [(AC0, "ac0"),
   (HI0, "hi0"),
   (LO0, "lo0")] ++
  regStringsWithIndex "f" FGR32Opnd ++
  regStringsWithIndex "d" AFGR64Opnd ++
  [(D09, "d09")] ++
  [(FCC0, "fcc0")] ++
  [(DSPCCond, "dspccond"),
   (DSPCarry, "dspcarry"),
   (DSPEFI, "dspefi"),
   (DSPOutFlag16_19, "dspoutflag16_19"),
   (DSPOutFlag20, "dspoutflag20"),
   (DSPOutFlag21, "dspoutflag21"),
   (DSPOutFlag22, "dspoutflag22"),
   (DSPOutFlag23, "dspoutflag23"),
   (DSPPos, "dsppos"),
   (DSPSCount, "dspscount"),
   (HI0_64, "hi0_64"),
   (LO0_64, "lo0_64"),
   (MPL0, "mpl0"),
   (MPL1, "mpl1"),
   (MPL2, "mpl2"),
   (P0, "p0"),
   (P1, "p1"),
   (P2, "p2")]

regStringsWithIndex pre rc =
  [(r, pre ++ show idx) | (r, idx) <- zip (registers (RegisterClass rc)) [0..]]
