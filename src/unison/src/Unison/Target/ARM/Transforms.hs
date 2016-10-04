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
module Unison.Target.ARM.Transforms
    (extractReturnRegs,
     foldResRegAssignment,
     cleanResRegCopies,
     addThumbAlternatives,
     handlePromotedOperands) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import Data.Bits

import Unison
import MachineIR
import Unison.Target.Query
import Unison.Target.ARM.Common
import Unison.Target.ARM.Registers
import Unison.Target.ARM.OperandInfo
import Unison.Target.ARM.Usages
import Unison.Target.ARM.ARMResourceDecl
import Unison.Target.ARM.SpecsGen.ARMInstructionDecl

extractReturnRegs _ (
  c1 @ SingleOperation {oOpr = Virtual (co1 @ VirtualCopy {
                                           oVirtualCopyD = Register r1})}
  :
  c2 @ SingleOperation {oOpr = Virtual (co2 @ VirtualCopy {
                                           oVirtualCopyD = Register r2})}
  :
  c3 @ SingleOperation {oOpr = Virtual (co3 @ VirtualCopy {
                                           oVirtualCopyD = Register r3})}
  :
  b @ SingleOperation {oOpr = Natural TailCall {}}
  :
  o @ SingleOperation {oOpr = Virtual (Delimiter Out {oOuts = outs})}
  :
  rest) (ti, _) | Register r1 `elem` outs &&
                  Register r2 `elem` outs &&
                  Register r3 `elem` outs =
  let t1 = mkTemp ti
      t2 = mkTemp (ti + 1)
      t3 = mkTemp (ti + 2)
      m = M.fromList [(Register r1, preAssign t1 (Register r1)),
                      (Register r2, preAssign t2 (Register r2)),
                      (Register r3, preAssign t3 (Register r3))]
  in
   (
    rest,
    [c1 {oOpr = Virtual co1 {oVirtualCopyD = t1}},
     c2 {oOpr = Virtual co2 {oVirtualCopyD = t2}},
     c3 {oOpr = Virtual co3 {oVirtualCopyD = t3}},
     b,
     mapToOperandIf isRegister (applyMap m) o]
   )

extractReturnRegs _ (
  c1 @ SingleOperation {oOpr = Virtual (co1 @ VirtualCopy {
                                           oVirtualCopyD = Register r1})}
  :
  c2 @ SingleOperation {oOpr = Virtual (co2 @ VirtualCopy {
                                           oVirtualCopyD = Register r2})}
  :
  b @ SingleOperation {oOpr = Natural TailCall {}}
  :
  o @ SingleOperation {oOpr = Virtual (Delimiter Out {oOuts = outs})}
  :
  rest) (ti, _) | Register r1 `elem` outs &&
                  Register r2 `elem` outs =
  let t1 = mkTemp ti
      t2 = mkTemp (ti + 1)
      m = M.fromList [(Register r1, preAssign t1 (Register r1)),
                      (Register r2, preAssign t2 (Register r2))]
  in
   (
    rest,
    [c1 {oOpr = Virtual co1 {oVirtualCopyD = t1}},
     c2 {oOpr = Virtual co2 {oVirtualCopyD = t2}},
     b,
     mapToOperandIf isRegister (applyMap m) o]
   )

extractReturnRegs _ (o : rest) _ = (rest, [o])

foldResRegAssignment f (o : rest) _ =
  let fcode = flatCode f
      o'    = mapToOperandIf (isReservedTemp fcode) (foldTempReg fcode) o
  in (rest, [o'])

isReservedTemp fcode t @ Temporary {} =
  case definer t fcode of
    SingleOperation {
      oOpr = Virtual (VirtualCopy {oVirtualCopyS = r @ Register {}})}
      | isReservedReg r -> True
    _ ->  False
isReservedTemp _ _ = False

isReservedReg r = rTargetReg (regId r) `elem` reserved

foldTempReg fcode t = oSingleUse $ definer t fcode

cleanResRegCopies _ (
  SingleOperation {oOpr = Virtual (VirtualCopy {oVirtualCopyS = r,
                                                oVirtualCopyD = r'})}
  :
  rest) _ | isRegister r && isRegister r' && r == r' && isReservedReg r =
  (rest, [])

cleanResRegCopies _ (o : rest) _ = (rest, [o])

-- This transformation adds 16-bits Thumb alternatives when possible, according
-- to the logic in Thumb2SizeReduction

addThumbAlternatives goal o @ SingleOperation {
  oOpr = Natural Linear {oIs = [TargetInstruction i]}} =
  let o' = addThumbAlternative o (M.lookup i reduceMap) i
  in case goal of
      Size -> o'
      -- if we optimize for speed, keep on Thumb alternatives if they can
      -- improve latency
      Speed -> if occupations o == occupations o' then o else o'
addThumbAlternatives _ o = o

occupations = S.fromList .
              concatMap (map occupation . filter isV6 . usages . oTargetInstr) .
              oInstructions

isV6 Usage {resource = V6_Pipe} = True
isV6 _ = False

addThumbAlternative o (Just r) i
  | special r = reduceSpecial o r i
  | otherwise =
    let o1 = if narrowOpc1 r /= NOP then reduceToNarrow o  r else o
        o2 = if narrowOpc2 r /= NOP then reduceTo2Addr  o1 r else o1
    in o2
addThumbAlternative o Nothing _ = o

-- See 'ReduceSpecial' in Thumb2SizeReduction.cpp
reduceSpecial o r i
  | i `elem` [T2ADDri] = addThumbAlternative o (Just r {special = False}) i
  | i `elem` [T2LDRi12, T2STRi12, T2LDRBi12, T2STRBi12, T2LDRHi12, T2STRHi12,
              T2LDRs, T2LDRBs, T2LDRHs, T2LDRSBs, T2LDRSHs, T2STRs, T2STRBs,
              T2STRHs, T2LDMIA, T2STMIA, T2LDMIA_RET, T2LDMIA_UPD, T2STMIA_UPD,
              T2STMDB_UPD] = reduceLoadStore o r i
  | i `elem` [T2ADDSri, T2ADDSrr] = error ("TODO: implement reduceSpecial case")
  | i `elem` [T2RSBri, T2RSBSri, T2SXTB, T2SXTH, T2UXTB, T2UXTH] =
    case (oUses o !! 1) of
      Bound (MachineImm 0) -> reduceToNarrow o r
      _ -> o
  | i `elem` [T2MOVi16] =
    case (oUses o !! 0) of
      Bound (MachineImm {}) -> reduceToNarrow o r
      _ -> o
  | i `elem` [T2CMPrr] =
    let o1 = reduceToNarrow o (r {narrowOpc1 = TCMPr})
        o2 = reduceToNarrow o1 r
    in o2
  | otherwise = o

-- See 'ReduceLoadStore' in Thumb2SizeReduction.cpp

data LSParameters = LSParameters {
  scale        :: Word32,
  hasImmOffset :: Bool,
  hasShift     :: Bool,
  hasOffReg    :: Bool,
  isLdStMul    :: Bool,
  opc          :: ARMInstruction,
  opNum        :: Integer,
  immLimit     :: Integer
  } deriving Show

reduceLoadStore o r i =
  let ps = updateLSParameters
           (LSParameters 1 False False True False
            (narrowOpc1 r) 3 (imm1Limit r))
           o r i
  in maybeReduceLoadStore ps o r i

updateLSParameters ps _ r i
  | i `elem` [T2LDRi12, T2STRi12] =
    ps {scale = 4, hasImmOffset = True, hasOffReg = False}
  | i `elem` [T2LDRBi12, T2STRBi12] =
    ps {hasImmOffset = True, hasOffReg = False}
  | i `elem` [T2LDRHi12, T2STRHi12] =
    ps {scale = 2, hasImmOffset = True, hasOffReg = False}
  | i `elem` [T2LDRs, T2LDRBs, T2LDRHs, T2LDRSBs, T2LDRSHs, T2STRs, T2STRBs,
              T2STRHs] =
    ps {hasShift = True, opNum = 4}
  | i `elem` [T2LDMIA] =
    ps {opNum = 0, isLdStMul = True}
  | i `elem` [T2STMIA] = ps
  | i `elem` [T2LDMIA_RET] =
    ps {opc = narrowOpc2 r, opNum = 2, isLdStMul = True}
  | i `elem` [T2LDMIA_UPD, T2STMDB_UPD] =
    ps {opc = narrowOpc2 r, opNum = 2, isLdStMul = True}
  | i `elem` [T2STMIA_UPD] =
    ps {opNum = 0, isLdStMul = True}
  | otherwise = error ("unmatched: updateLSParameters " ++ show i)

maybeReduceLoadStore ps o r i = maybeReduceLoadStore1 ps o r i

maybeReduceLoadStore1 ps o _ i
  | hasShift ps && (loadStoreShiftImm o i) > 0 = o
  | hasImmOffset ps && outOfLimit (loadStoreImm o i) ps = o
  -- Otherwise, we are good to add the Thumb alternative
  | otherwise = mapToInstructions (\is -> [TargetInstruction $ opc ps] ++ is) o

loadStoreImm o i
  | i `elem` [T2LDRi12, T2LDRBi12, T2LDRHi12] = imm (oUses o !! 1)
  | i `elem` [T2STRi12, T2STRBi12, T2STRHi12] = imm (oUses o !! 2)

loadStoreShiftImm o i
  | i `elem` [T2LDRs, T2LDRBs, T2LDRHs, T2LDRSBs, T2LDRSHs] =
    imm (oUses o !! 2)
  | i `elem` [T2STRs, T2STRBs, T2STRHs] = imm (oUses o !! 3)

outOfLimit offsetImm ps =
  let maxOffset = (computeLimit (immLimit ps)) * (scale ps)
      offsetImm' = fromIntegral offsetImm :: Word32
  in (offsetImm' .&. (scale ps - 1)) /= 0 || offsetImm' > maxOffset

imm :: Operand r -> Integer
imm (Bound (MachineImm i)) = i

-- See 'ReduceToNarrow' in Thumb2SizeReduction.cpp

-- possible to reduce if:
--   all non-predicate immediate operands are <= imm1Limit (in unsigned mode)
--   AND
--   if i contains a predicate, the narrow instruction is predicable
reduceToNarrow o r =
  let limit = computeLimit (imm1Limit r)
      us    = realUses (oUses o)
  in if any (exceedsImmLimit limit) us then o
     else mapToInstructions (\is -> [TargetInstruction $ narrowOpc1 r] ++ is) o

-- See 'ReduceTo2Addr' in Thumb2SizeReduction.cpp

-- possible to reduce if:
--   all non-predicate immediate operands are <= imm2Limit (in unsigned mode)
--   AND
--   if i contains a predicate, the narrow instruction is predicable
reduceTo2Addr o r =
  let limit = computeLimit (imm2Limit r)
      us    = realUses (oUses o)
  in if any (exceedsImmLimit limit) us then o
     else mapToInstructions (\is -> [TargetInstruction $ narrowOpc2 r] ++ is) o

computeLimit immLimit =
  let l = fromIntegral immLimit :: Int
  in if immLimit > 0 then
       fromIntegral (shiftL (1 :: Word32) l) - 1 :: Word32
     else 0

realUses [Temporary {}, Temporary {}, cond, _, _]
  | isImmValue 14 cond = []
realUses [Temporary {}, imm1, Bound {}, cs, _]
  | not (isBound cs) = [imm1]
realUses [Temporary {}, imm1, cond, _, _]
  | isImmValue 14 cond = [imm1]
realUses [Temporary {}, imm1, cond, _]
  | isImmValue 14 cond = [imm1]
realUses [imm1, cond, _, _]
  | isImmValue 14 cond = [imm1]
realUses [imm1, cond, _]
  | isImmValue 14 cond = [imm1]
realUses [Temporary {}, Temporary {}, cond, r]
  | isImmValue 0 cond = []
  | isImmValue 12 cond = []
  | isImmValue 14 cond = []
  | isRegister r = []
realUses [imm1, _, r, _]
  | isRegister r = [imm1]
realUses [imm1, cond, Temporary {}, _]
  | isImmValue 1 cond = [imm1]
  | isImmValue 10 cond = [imm1]
realUses us = error ("unmatched: realUses " ++ show us)

isImmValue n (Bound (MachineImm imm)) = n == imm
isImmValue _ _ = False

exceedsImmLimit limit (Bound (MachineImm imm)) =
  (fromIntegral imm :: Word32) > limit
exceedsImmLimit _ _ = False

data ReduceEntry = ReduceEntry {
  -- NarrowOpc1: Narrow opcode to transform to (NOP means 0)
  narrowOpc1 :: ARMInstruction,
  -- NarrowOpc2:   Narrow opcode when it's two-address (NOP means 0)
  narrowOpc2 :: ARMInstruction,
  -- Imm1Limit:    Limit of immediate field (bits)
  imm1Limit :: Integer,
    -- Imm2Limit:    Limit of immediate field when it's two-address
  imm2Limit :: Integer,
    -- LowRegs1 : 1: Only possible if low-registers are used
  _lowRegs1 :: Bool,
    -- LowRegs2 : 1: Only possible if low-registers are used (2addr)
  _lowRegs2 :: Bool,
    -- PredCC1  :    0 - If predicated, cc is on and vice versa.
    --               1 - No cc field.
    --               2 - Always set CPSR.
  _predCC1 :: Integer,
    -- PredCC2  :
  _predCC2 :: Integer,
    -- PartFlag : 1: 16-bit instruction does partial flag update
  _partFlag :: Bool,
    -- Special  : 1: Needs to be dealt with specially
  special :: Bool,
    -- AvoidMovs: 1: Avoid movs with shifter operand (for Swift)
  _avoidMovs :: Bool
  } deriving Show

reduceMap = M.fromList [(w, ReduceEntry n1 n2 imm1 imm2 (i2b lo1) (i2b lo2)
                            p c (i2b pf) (i2b s) (i2b am))
                       | (w,n1,n2,imm1,imm2,lo1,lo2,p,c,pf,s,am) <- reduceTable]

-- A static table with information on mapping from wide opcodes to narrow. Some
-- instructions have been slighly modified to be operand-compatible (e.g. TADDi8
-- -> TADDi8s ('s' operand use only); TMUL -> TMULz (zero-immediate))
reduceTable =
--  Wide,    Narrow1, Narrow2,   imm1,imm2,lo1, lo2,P/C,PF,S,AM
 [( T2ADCrr, NOP,     TADC,      0,   0,   0,   1,  0,0, 0,0,0 ),
  ( T2ADDri, TADDi3s, TADDi8s,   3,   8,   1,   1,  0,0, 0,1,0 ),
  ( T2ADDrr, TADDrrs, TADDhirrs, 0,   0,   1,   0,  0,1, 0,0,0 ),
  ( T2ADDSri,TADDi3s, TADDi8s,   3,   8,   1,   1,  2,2, 0,1,0 ),
  ( T2ADDSrr,TADDrr,  NOP,       0,   0,   1,   0,  2,0, 0,1,0 ),
  ( T2ANDrr, NOP,     TANDs,     0,   0,   0,   1,  0,0, 1,0,0 ),
  ( T2ASRri, TASRris, NOP,       5,   0,   1,   0,  0,0, 1,0,1 ),
  ( T2ASRrr, NOP,     TASRrrs,   0,   0,   0,   1,  0,0, 1,0,1 ),
  ( T2BICrr, NOP,     TBICs,     0,   0,   0,   1,  0,0, 1,0,0 ),
  ( T2CMNzrr, TCMNz,  NOP,       0,   0,   1,   0,  2,0, 0,0,0 ),
  ( T2CMPri, TCMPi8,  NOP,       8,   0,   1,   0,  2,0, 0,0,0 ),
  ( T2CMPrr, TCMPhir, NOP,       0,   0,   0,   0,  2,0, 0,1,0 ),
  ( T2EORrr, NOP,     TEORs,     0,   0,   0,   1,  0,0, 1,0,0 ),
  ( T2LSLri, TLSLris, NOP,       5,   0,   1,   0,  0,0, 1,0,1 ),
  ( T2LSLrr, NOP,     TLSLrrs,   0,   0,   0,   1,  0,0, 1,0,1 ),
  ( T2LSRri, TLSRris, NOP,       5,   0,   1,   0,  0,0, 1,0,1 ),
  ( T2LSRrr, NOP,     TLSRrrs,   0,   0,   0,   1,  0,0, 1,0,1 ),
  ( T2MOVi,  TMOVi8s, NOP,       8,   0,   1,   0,  0,0, 1,0,0 ),
  ( T2MOVi16,TMOVi8,  NOP,       8,   0,   1,   0,  0,0, 1,1,0 ),
  ( T2MOVr,  TMOVr,   NOP,       0,   0,   0,   0,  1,0, 0,0,0 ),
  ( T2MUL,   NOP,     TMULz,     0,   0,   0,   1,  0,0, 1,0,0 ),
  ( T2MVNr,  TMVNs,   NOP,       0,   0,   1,   0,  0,0, 0,0,0 ),
  ( T2ORRrr, NOP,     TORRs,     0,   0,   0,   1,  0,0, 1,0,0 ),
  ( T2REV,   TREV,    NOP,       0,   0,   1,   0,  1,0, 0,0,0 ),
  ( T2REV16, TREV16,  NOP,       0,   0,   1,   0,  1,0, 0,0,0 ),
  ( T2REVSH, TREVSH,  NOP,       0,   0,   1,   0,  1,0, 0,0,0 ),
  ( T2RORrr, NOP,     TROR,      0,   0,   0,   1,  0,0, 1,0,0 ),
  ( T2RSBri, TRSBs,   NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2RSBSri,TRSBs,   NOP,       0,   0,   1,   0,  2,0, 0,1,0 ),
  ( T2SBCrr, NOP,     TSBC,      0,   0,   0,   1,  0,0, 0,0,0 ),
  ( T2SUBri, TSUBi3s, TSUBi8s,   3,   8,   1,   1,  0,0, 0,0,0 ),
  ( T2SUBrr, TSUBrrs, NOP,       0,   0,   1,   0,  0,0, 0,0,0 ),
  ( T2SUBSri,TSUBi3s, TSUBi8s,   3,   8,   1,   1,  2,2, 0,0,0 ),
  ( T2SUBSrr,TSUBrr,  NOP,       0,   0,   1,   0,  2,0, 0,0,0 ),
  ( T2SXTB,  TSXTB,   NOP,       0,   0,   1,   0,  1,0, 0,1,0 ),
  ( T2SXTH,  TSXTHz,  NOP,       0,   0,   1,   0,  1,0, 0,1,0 ),
  ( T2TSTrr, TTST,    NOP,       0,   0,   1,   0,  2,0, 0,0,0 ),
  ( T2UXTB,  TUXTBz,  NOP,       0,   0,   1,   0,  1,0, 0,1,0 ),
  ( T2UXTH,  TUXTHz,  NOP,       0,   0,   1,   0,  1,0, 0,1,0 ),
  ( T2LDRi12,TLDRi,   TLDRspi,   5,   8,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRs,  TLDRrz,  NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRBi12,TLDRBi, NOP,       5,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRBs, TLDRBrz, NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRHi12,TLDRHi, NOP,       5,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRHs, TLDRHrz, NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRSBs,TLDRSB,  NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDRSHs,TLDRSHz, NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2STRi12,TSTRi,   TSTRspi,   5,   8,   1,   0,  0,0, 0,1,0 ),
  ( T2STRs,  TSTRrz,  NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2STRBi12,TSTRBi, NOP,       5,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2STRBs, TSTRBrz, NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2STRHi12,TSTRHi, NOP,       5,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2STRHs, TSTRHrz, NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
  ( T2LDMIA, TLDMIA,  NOP,       0,   0,   1,   1,  1,1, 0,1,0 ),
  ( T2LDMIA_RET,NOP,  TPOP_RET,  0,   0,   1,   1,  1,1, 0,1,0 ),
  ( T2LDMIA_UPD,TLDMIA_UPD,TPOP, 0,   0,   1,   1,  1,1, 0,1,0 )]

i2b 0 = False
i2b 1 = True

handlePromotedOperands _ (
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = is, oDs = [t]})}
  :
  rest) (_, _) | isTemporary t &&
                 all (\(TargetInstruction i) -> isCpsrDef i) is &&
                 isAbstractRegisterClass (regClassOf operandInfo o t) =
    (
     rest,
     [o {oOpr = Natural ni {oIs = map (\(TargetInstruction i) ->
                                        TargetInstruction (toExplicitCpsrDef i))
                                  is}}]
    )

handlePromotedOperands _ (o : rest) _ = (rest, [o])
