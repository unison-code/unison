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
module Unison.Target.Hexagon.Registers
    (registerArray, registerAtoms, regClasses, registers,
     hexagonSP, subRegIndexType, infRegClassUsage, infRegClassBound,
     reserved, callerSaved, calleeSaved) where

import qualified Data.Map as M

import Unison
import Unison.Target.Hexagon.HexagonRegisterDecl
import Unison.Target.Hexagon.HexagonRegisterClassDecl

-- | Register array

registerArray =
  [RegisterClass IntRegs, RegisterClass PredRegs, RegisterClass F32,
   InfiniteRegisterClass M32, InfiniteRegisterClass RM32]


-- | Register atoms of 1-width registers

registerAtoms ra
    | ra `elem` concatMap (registers . RegisterClass) [IntRegs, PredRegs, F32] =
        (ra, ra)

-- | Register atoms of 2-width registers

registerAtoms (D0)  = (R0, R1)
registerAtoms (D1)  = (R2, R3)
registerAtoms (D2)  = (R4, R5)
registerAtoms (D3)  = (R6, R7)
registerAtoms (D4)  = (R8, R9)
registerAtoms (D5)  = (R10, R11)
registerAtoms (D6)  = (R12, R13)
registerAtoms (D7)  = (R14, R15)
registerAtoms (D8)  = (R16, R17)
registerAtoms (D9)  = (R18, R19)
registerAtoms (D10) = (R20, R21)
registerAtoms (D11) = (R22, R23)
registerAtoms (D12) = (R24, R25)
registerAtoms (D13) = (R26, R27)
registerAtoms (D14) = (R28, R29)
registerAtoms (D15) = (R30, R31)

registerAtoms (P0_1) = (P0, P1)
registerAtoms (P2_3) = (P2, P3)

-- | Register atoms of 4-width registers

registerAtoms (D0_1)   = (R0,  R3)
registerAtoms (D2_3)   = (R4,  R7)
registerAtoms (D4_5)   = (R8,  R11)
registerAtoms (D6_7)   = (R12, R15)
registerAtoms (D8_9)   = (R16, R19)
registerAtoms (D10_11) = (R20, R23)
registerAtoms (D12_13) = (R24, R27)
registerAtoms (D14_15) = (R28, R31)

registerAtoms (P0_3) = (P0,  P3)

-- | Register atoms of 8-width registers

registerAtoms (D0_3)   = (R0,  R7)
registerAtoms (D4_7)   = (R8,  R15)
registerAtoms (D8_11)  = (R16, R23)
registerAtoms (D12_15) = (R24, R31)

-- | Register classes
regClasses =
    map RegisterClass
            [IntRegs, DoubleRegs, G128, G256, PredRegs, P64, P128, F32] ++
    map InfiniteRegisterClass  [M32, M64, RM32, RM64]

-- | Individual registers of each register class

registers (RegisterClass IntRegs) =
    [R0, R1, R2, R3, R4, R5, R6, R7, R8,
     R9, R10, R11, R12, R13, R14, R15, R16,
     R17, R18, R19, R20, R21, R22, R23, R24,
     R25, R26, R27, R28, R29, R30, R31]

registers (RegisterClass DoubleRegs) =
    [D0, D1, D2, D3, D4, D5, D6, D7,
     D8, D9, D10, D11, D12, D13, D14, D15]

-- These register classes are defined to allow a more compact representation of
-- register groups:

registers (RegisterClass G128) =
  [D0_1, D2_3, D4_5, D6_7, D8_9, D10_11, D12_13, D14_15]

registers (RegisterClass G256) = [D0_3, D4_7, D8_11, D12_15]

registers (RegisterClass PredRegs) = [P0, P1, P2, P3]

-- Same as above:

registers (RegisterClass P64) = [P0_1, P2_3]

registers (RegisterClass P128) = [P0_3]

registers (RegisterClass F32) = [F0]

hexagonSP = R29

-- | Index type (low/high/copy) of subregisters

subRegIndexType _ sr
    | sr == (NamedSubRegIndex "subreg_hireg") ||
      sr == (RawSubRegIndex 1) = [HighSubRegIndex]
subRegIndexType _ sr
    | sr == (NamedSubRegIndex "subreg_loreg") ||
      sr == (RawSubRegIndex 2) = [LowSubRegIndex]
subRegIndexType _ subreg = error ("unmatched: subRegIndexType " ++ show subreg)

-- | Map from infinite register class to register usage

infRegClassUsage (InfiniteRegisterClass rc)
  | rc == M32 = 1
  | rc == M64 = 2
  | rc == RM32 = 1
  | rc == RM64 = 2

-- | Map from infinite register class to (possibly) register atom upper bound

infRegClassBound _ = Nothing

-- | Registers whose value cannot be moved around

reserved = [R29, R30, R31]

-- | Caller- and callee-saved registers

-- | Registers that are not preserved across calls
-- | see CALLv3 instruction in HexagonInstrInfoV3.td
callerSaved =
    [R0, R1, R2, R3, R4, R5, R6, R7, R8,
     R9, R10, R11, R12, R13, R14, R15, R28,
     P0, P1, P2, P3]

-- | Registers that are preserved across calls
-- | see getCalleeSavedRegs() function in HexagonRegisterInfo.cpp
calleeSaved =
    [D8, D9, D10, D11, D12, D13]

instance Read HexagonRegister where
  readsPrec _ s = [(readReg s, "")]

readReg s = case M.lookup s (inverseMap regStrings) of
              (Just r) -> r
              Nothing -> error $ "unmatched: readReg " ++ s

instance Show HexagonRegister where
  show r = case M.lookup r regStrings of
             (Just s) -> s
             Nothing -> error $ "unmatched: show HexagonRegister"

regStrings = M.fromList $
  regStringsWithIndex 1 "r" IntRegs ++
  regStringsWithIndex 1 "d" DoubleRegs ++
  regStringsWithIndex 2 "d" G128 ++
  regStringsWithIndex 4 "d" G256 ++
  regStringsWithIndex 1 "p" PredRegs ++
  regStringsWithIndex 2 "p" P64 ++
  regStringsWithIndex 4 "p" P128 ++
  [(F0, "f0")] ++
  [(PC, "pc"),
   (M0, "m0"),
   (M1, "m1"),
   (LC0, "lc0"),
   (LC1, "lc1"),
   (SA0, "sa0"),
   (SA1, "sa1"),
   (USR, "usr"),
   (USR_OVF, "usr_ovf"),
   (CS, "cs"),
   (GP, "gp")]

regStringsWithIndex inc pre rc =
  [(r, pre ++ show idx ++
    if inc == 1 then "" else ("_" ++ show (idx + inc - 1)))
   | (r, idx) <- zip (registers (RegisterClass rc)) [0, inc..]]
