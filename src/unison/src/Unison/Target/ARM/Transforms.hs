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
module Unison.Target.ARM.Transforms
    (extractReturnRegs,
     addThumbAlternatives,
     expandMEMCPY,
     handlePromotedOperands,
     defineFP,
     combinePushPops,
     expandRets,
     normalizeLoadStores,
     combineLoadStores,
     reorderCalleeSavedSpills,
     enforceStackFrame) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import Data.Bits
import Data.List
import Data.Maybe

import Common.Util
import Unison
import MachineIR
import Unison.Target.Query
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.Partition as P
import Unison.Target.ARM.Common
import Unison.Target.ARM.OperandInfo
import Unison.Target.ARM.Usages
import Unison.Target.ARM.ARMResourceDecl
import Unison.Target.ARM.ARMRegisterDecl
import Unison.Target.ARM.SpecsGen.ARMInstructionDecl
import Unison.Target.ARM.Registers()

extractReturnRegs _ (
  c
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter oi @ (Out {oOuts = outs}))}
  :
  rest) _ | isTailCall c && all isRegister outs =
   (
    rest,
    [c,
     o {oOpr = Virtual (Delimiter oi {oOuts = []})}]
   )

extractReturnRegs _ (o : rest) _ = (rest, [o])

-- This transformation adds 16-bits Thumb alternatives when possible, according
-- to the logic in Thumb2SizeReduction

addThumbAlternatives goals o @ SingleOperation {
  oOpr = Natural Linear {oIs = [TargetInstruction i]}} =
  let o' = addThumbAlternative o (M.lookup i reduceMap) i
  in if none ((==) Size) goals then
       -- if we do not optimize for size, keep on Thumb alternatives only
       -- if they can improve latency
       (if occupations o == occupations o' then o else o')
     else o'
addThumbAlternatives _ o = o

occupations =
  S.fromList .
  concatMap (map occupation . filter isV6 . usages [] . oTargetInstr) .
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
  ( T2RORrr, NOP,     TRORs,     0,   0,   0,   1,  0,0, 1,0,0 ),
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
  ( T2LDRSBs,TLDRSBz, NOP,       0,   0,   1,   0,  0,0, 0,1,0 ),
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

{- see expandMEMCPY in ARMBaseInstrInfo.cpp:
    [dst',src',r0,r1,r2,r3] <- MEMCPY_4 [dst,src,4]
    ->
    [[src',]r0,r1,r2,r3] <- T2LDMIA[UPD]_4 [src]
    [[dst']] <- T2STMIA[UPD]_4 [dst,14,_,r0,r1,r2,r3]
-}

expandMEMCPY f (
  SingleOperation {oOpr = Natural Linear {
                      oIs = [TargetInstruction MEMCPY_4],
                      oUs = [dst, src, _],
                      oDs = [dst', src', r0, r1, r2, r3]}}
  :
  rest) (_, oid, _) =
  let isUsed t = any (isUser t) (flatCode f)
      (ldmi, ldmds) =
        if isUsed src' then (T2LDMIA_UPD_4, [src']) else (T2LDMIA_4, [])
      (stmi, stmds) =
        if isUsed dst' then (T2STMIA_UPD_4, [dst']) else (T2STMIA_4, [])
      scratch = [r0, r1, r2, r3]
  in
   (
     rest,
     [mkLinear oid       [TargetInstruction ldmi]
      ([src] ++ defaultUniPred) (ldmds ++ scratch),
      mkLinear (oid + 1) [TargetInstruction stmi]
      ([dst] ++ defaultUniPred ++ scratch) stmds]
   )

expandMEMCPY _ (o : rest) _ = (rest, [o])

handlePromotedOperands _ (
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = is, oDs = [t]})}
  :
  rest) _ | isTemporary t &&
            all (\(TargetInstruction i) -> isCpsrDef i) is &&
            isAbstractRegisterClass (regClassOf operandInfo o t) =
    (
     rest,
     [o {oOpr = Natural ni {oIs = map (\(TargetInstruction i) ->
                                        TargetInstruction (toExplicitCpsrDef i))
                                  is}}]
    )

handlePromotedOperands _ (o : rest) _ = (rest, [o])

defineFP f @ Function {fCode = code} =
  let fcode = flatten code
      isRegFun o = isFun o && not (isTailCallFun fcode o)
  in if any isRegFun fcode then
       let (tid, oid, _) = newIndexes fcode
           pt    = mkPreAssignedTemp tid (mkRegister $ mkTargetRegister R7)
           o     = mkLinear oid [TargetInstruction TFP]
                   ([mkRegister (mkTargetRegister SP),
                     mkBound (mkMachineImm 0)] ++ defaultUniPred) [pt]
           code' = mapToEntryBlock (insertWhen after isIn [o]) code
           f'    = mapToOperation (applyIf isRegFun (addOperands [pt] []))
                   f {fCode = code'}
       in f'
     else f

{-
 Transforms:
    o1: [p1{ -, t1}]  <- { -, tPUSH/POP_r4_7}  [p0{ -, t0}]
    o2: [p3{ -, t3}]  <- { -, tPUSH/POP_r8_11} [p2{ -, t2}]
 into:
    o3: [p1{ -, t1}, p3{ -, t3}]  <- { -, tPUSH/POP2_r4_7, tPUSH/POP2_r4_11}  [p0{ -, t0}, p2{ -, t2}]
-}

combinePushPops _ (
  SingleOperation {
     oOpr = Copy {oCopyIs = [General NullInstruction,
                             TargetInstruction i1],
                  oCopyS = p0, oCopyD = p1}}
  :
  SingleOperation {
     oOpr = Copy {oCopyIs = [General NullInstruction,
                             TargetInstruction i2],
                  oCopyS = p2, oCopyD = p3}}
  :
  rest) (_, oid, _) | all isTPush [i1, i2] || all isTPop [i1, i2] =
  let is = [General NullInstruction] ++
           map TargetInstruction
           (if all isTPush [i1, i2]
            then [TPUSH2_r4_7, TPUSH2_r4_11]
            else [TPOP2_r4_7,  TPOP2_r4_11])
      o3 = mkLinear oid is [p0, p2] [p1, p3]
  in (rest, [o3])

combinePushPops _ (o : code) _ = (code, [o])

isTPush i = i `elem` [TPUSH_r4_7, TPUSH_r8_11]
isTPop  i = i `elem` [TPOP_r4_7,  TPOP_r8_11]

{-
 Transforms:
  bN (.., return, ..):
    op: [p1, p3]  <- { -, tPOP2_r4_7, tPOP2_r4_11}  [p0, p2]
    (..)
    or: [] <- tBX_RET [14,_]
 into:
  bN (.., return, ..):
    op: [p1, p3]  <- { -, tPOP2_r4_7_RET, tPOP2_r4_11_RET}  [p0, p2]
    or: [] <- { -, tBX_RET} [14,_]
-}

expandRets _ (
  op @ SingleOperation {
     oOpr = Natural Linear {oIs = [General NullInstruction,
                                   TargetInstruction TPOP2_r4_7,
                                   TargetInstruction TPOP2_r4_11]}}
  :
  code) _ =
  case find isTRET code of
   Just or ->
     let opis  = [General NullInstruction,
                  TargetInstruction TPOP2_r4_7_RET,
                  TargetInstruction TPOP2_r4_11_RET]
         op'   = mapToInstructions (const opis) op
         or'   = makeOptional or
         code' = concatMap (\o -> if isTRET o then [op', or'] else [o]) code
     in ([], code')
   Nothing -> (code, [op])

expandRets _ (o : code) _ = (code, [o])

isTRET = isMandNaturalWith ((==) TBX_RET)

-- Move remats out of store sequences to enable matching of 'combineLoadStores'

normalizeLoadStores _ (c1 : c2 : r : s : rest) _
  | all isCopy [c1, c2] && isRemat r && isSingleStore s = (rest, [r, c1, c2, s])

normalizeLoadStores _ (s : r : rest) _
  | isRemat r && isSingleStore s = (rest, [r, s])

normalizeLoadStores _ (o : code) _ = (code, [o])

isSingleStore = isMandNaturalWith ((==) T2STRi12)

{-
 Transforms:
    o11: [p37{ -, t25}] <- { -, MOVE_ALL, LOAD} [p36{ -, t0, t14}]
    o12: [p39{t26}] <- t2LDRi12 [p38{t0, t14, t25, t28, t54},12,14,_]
    o13: [p41{ -, t27}] <- { -, MOVE_ALL, STORE} [p40{ -, t26}]
    o14: [p43{ -, t28}] <- { -, MOVE_ALL, LOAD} [p42{ -, t0, t14}]
    o15: [p45{t29}] <- t2LDRi12 [p44{t0, t14, t25, t28, t54},16,14,_]
    o16: [p47{ -, t30}] <- { -, MOVE_ALL, STORE} [p46{ -, t29}]
 into:
    o11: [p37{ -, t25}] <- { -, MOVE_ALL, LOAD} [p36{ -, t0, t14}]
    o14: [p43{ -, t28}] <- { -, MOVE_ALL, LOAD} [p42{ -, t0, t14}]
    o12: [p39{ -, t26'}] <- { - , t2LDRi12} [p38{ -, t0, t14, t25, t28, t54},12,14,_]
    o15: [p45{ -, t29'}] <- { - , t2LDRi12} [p44{ -, t0, t14, t25, t28, t54},16,14,_]
    od:  [p101{ -, t26''}, p102{ -, t29''}] <- { - , t2LDRDi8} [p100{ -, t0, t14, t25, t28, t54},12,14,_]
    om1: [p104{t26} <- load_merge p103{t26', t26''}]
    o13: [p41{ -, t27}] <- { -, MOVE_ALL, STORE} [p40{ -, t26}]
    om2: [p106{t29} <- load_merge p105{t29', t29''}]
    o16: [p47{ -, t30}] <- { -, MOVE_ALL, STORE} [p46{ -, t29}]
 (see 'LoadStoreMultipleOpti' in ARMLoadStoreOptimizer.cpp)
 -- TODO: perform equivalent transformation for 't2STRDi8'
    (see e.g. hmmer.tophits.AllocFancyAli or sphinx3.profile.ptmr_init)
-}

combineLoadStores _ (
  uc1
  :
  ld1 @ SingleOperation {oOpr = Natural Linear {
                            oIs = ld1is,
                            oUs = MOperand {altTemps = uts1} : off1 : pred1,
                            oDs = [MOperand {altTemps = [tr1]}]}}
  :
  dc1
  :
  uc2
  :
  ld2 @ SingleOperation {oOpr = Natural Linear {
                            oIs = ld2is,
                            oUs = MOperand {altTemps = uts2} : off2 : pred2,
                            oDs = [MOperand {altTemps = [tr2]}]}}
  :
  dc2
  :
  rest) (tid, oid, pid)
  -- TODO: check offset (lines 2039-2042 in ARMLoadStoreOptimizer.cpp)
  | all isCombinableLoad [ld1is, ld2is] && all isCopy [uc1, dc1, uc2, dc2] &&
    ld1is == ld2is && uts1 == uts2 && pred1 == pred2 && offBy 4 off1 off2 =
  let mkOper = mkOperand pid
      replaceDefTempBy t = mapToOperands id (applyToAltTemps (const [t]))
      [tr1', tr1'', tr2', tr2''] = map (\id -> mkTemp (tid + id)) [0..3]
      ld1' = makeOptional $ replaceDefTempBy tr1' ld1
      ld2' = makeOptional $ replaceDefTempBy tr2' ld2
      ldd  = makeOptional $
             mkLinear oid       [TargetInstruction T2LDRDi8]
             (mkOper 0 uts1 : off1 : pred1) [mkOper 1 [tr1''], mkOper 2 [tr2'']]
      om1  = mkLinear (oid + 1) [TargetInstruction Load_merge]
             [mkOper 3 [tr1', tr1'']] [mkOper 4 [tr1]]
      om2  = mkLinear (oid + 2) [TargetInstruction Load_merge]
             [mkOper 5 [tr2', tr2'']] [mkOper 6 [tr2]]
  in (
      rest,
      [uc1, uc2, ld1', ld2', ldd, om1, dc1, om2, dc2]
     )

{-
 Transforms:
    o45: [p119{ -, t63}] <- { -, MOVE_ALL, LOAD} [p118{ -, t33, t46}]
    o46: [p121{ -, t64}] <- { -, MOVE_ALL, LOAD} [p120{ -, t22, t48}]
    o48: [] <- t2STRi12 [p123{t22, .., t65, ..},p124{t33, ..},24,14,_]
    o49: [p126{ -, t66}] <- { -, MOVE_ALL, LOAD} [p125{ -, t33, t46}]
    o50: [p128{ -, t67}] <- { -, MOVE_ALL, LOAD} [p127{ -, t47, t48}]
    o52: [] <- t2STRi12 [p130{t47, .., t65, .., t68, ..},p131{t33, ..},28,14,_]
 into:
    o45: [p119{ -, t63}] <- { -, MOVE_ALL, LOAD} [p118{ -, t33, t46}]
    o46: [p121{ -, t64}] <- { -, MOVE_ALL, LOAD} [p120{ -, t22, t48}]
    o49: [p126{ -, t66}] <- { -, MOVE_ALL, LOAD} [p125{ -, t33, t46}]
    o50: [p128{ -, t67}] <- { -, MOVE_ALL, LOAD} [p127{ -, t47, t48}]
    o48: [] <- { -, t2STRi12} [p123{ -, t22, .., t65, ..},p124{ -, t33, ..},24,14,_]
    o52: [] <- { -, t2STRi12} [p130{ -, t47, .., t65, .., t68, ..},p131{ -, t33, ..},28,14,_]
    od:  [] <- { -, t2STRDi8} [p105{ -, t22, ..}, p106{ -, t47, ..}, p107{ -, t33, ..}, 24, 14, _]
-}

combineLoadStores _ (
  uc11
  :
  uc12
  :
  st1 @ SingleOperation {oOpr = Natural Linear {
                            oIs = st1is,
                            oUs = MOperand {altTemps = ts1} :
                                  MOperand {altTemps = uts1} : off1 : pred1}}
  :
  uc21
  :
  uc22
  :
  st2 @ SingleOperation {oOpr = Natural Linear {
                            oIs = st2is,
                            oUs = MOperand {altTemps = ts2} :
                                  MOperand {altTemps = uts2} : off2 : pred2}}
  :
  rest) (_, oid, pid)
  -- TODO: check offset (lines 2039-2042 in ARMLoadStoreOptimizer.cpp)
  | all isCombinableStore [st1is, st2is] && all isCopy [uc11, uc21, uc21, uc22]
    && uts1 == uts2 && pred1 == pred2 && offBy 4 off1 off2 =
  let mkOper = mkOperand pid
      st1'   = makeOptional st1
      st2'   = makeOptional st2
      od     = makeOptional $ mkLinear oid
               [TargetInstruction T2STRDi8]
               (mkOper 5 ts1 : mkOper 6 ts2 : mkOper 7 uts1 : off1 : pred1)
               []
  in (
      rest,
      [uc11, uc12, uc21, uc22, st1', st2', od]
     )

{-
 Transforms:
    o35: [p87{ -, t50}] <- { -, MOVE_ALL, LOAD} [p86{ -, t48, t49}]
    o36: [] <- t2STRi12 [p88{t48, t49, t50},p89{t13},0,14,_]
    o37: [p91{ -, t51}] <- { -, MOVE_ALL, LOAD} [p90{ -, t44, t45}]
    o38: [] <- t2STRi12 [p92{t44, t45, t51, t53},p93{t13},4,14,_]
 into:
    o35: [p87{ -, t50}] <- { -, MOVE_ALL, LOAD} [p86{ -, t48, t49}]
    o37: [p91{ -, t51}] <- { -, MOVE_ALL, LOAD} [p90{ -, t44, t45}]
    o36: [] <- { -, t2STRi12} [p88{ -, t48, t49, t50},p89{ -, t13},0,14,_]
    o38: [] <- { -, t2STRi12} [p92{ -, t44, t45, t51, t53},p93{ -, t13},4,14,_]
    od:  [] <- { -, t2STRDi8} [p94{ -, t48, ..}, p95{ -, t44, ..}, p95{ -, t13}, 0, 14, _]
-}

combineLoadStores _ (
  uc1
  :
  st1 @ SingleOperation {oOpr = Natural Linear {
                            oIs = st1is,
                            oUs = MOperand {altTemps = ts1} :
                                  MOperand {altTemps = uts1} : off1 : pred1},
                         oAs = as}
  :
  uc2
  :
  st2 @ SingleOperation {oOpr = Natural Linear {
                            oIs = st2is,
                            oUs = MOperand {altTemps = ts2} :
                                  MOperand {altTemps = uts2} : off2 : pred2}}
  :
  rest) (_, oid, pid)
  -- TODO: check offset (lines 2039-2042 in ARMLoadStoreOptimizer.cpp)
  | all isCombinableStore [st1is, st2is] && all isCopy [uc1, uc2] &&
    uts1 == uts2 && pred1 == pred2 && offBy 4 off1 off2 =
  let mkOper = mkOperand pid
      st1'   = makeOptional st1
      st2'   = makeOptional st2
      od     = makeOptional $ (mkLinear oid
               [TargetInstruction T2STRDi8]
               (mkOper 5 ts1 : mkOper 6 ts2 : mkOper 7 uts1 : off1 : pred1)
               []) {oAs = as}
  in (
      rest,
      [uc1, uc2, st1', st2', od]
     )

combineLoadStores _ (o : code) _ = (code, [o])

isCombinableLoad  is = TargetInstruction T2LDRi12 `elem` is
isCombinableStore is = TargetInstruction T2STRi12 `elem` is

mkOperand pid id ts = mkMOperand (pid + id) ts Nothing

applyToAltTemps f [p @ MOperand {altTemps = ts}] = [p {altTemps = f ts}]

offBy n (Bound MachineImm {miValue = off1})
        (Bound MachineImm {miValue = off2}) =
  abs (off1 - off2) == n
offBy _ _ _ = False

reorderCalleeSavedSpills f @ Function {fCode = code}
  | any isReturnBlock code =
    let code1 = mapIf isEntryBlock (moveFP . reoderCalleeSavedStores) code
        code2 = mapIf isReturnBlock reoderCalleeSavedLoads code1
    in f {fCode = code2}
  | otherwise = f

reoderCalleeSavedStores b = moveOperations isCalleeSavedPrologue after isIn b

isCalleeSavedPrologue o =
  any (\i -> isInstr i o) [VSTMDDB_UPD_d8_15, TPUSH2_r4_7, TFP]

moveFP b
  | any isTPushDel (bCode b) = moveOperations (isInstr TFP) after isTPushDel b
  | otherwise = b

isTPushDel = isInstr TPUSH2_r4_7

reoderCalleeSavedLoads b =
  foldl (\b0 i -> moveOperations (isInstr i) before isTerm b0)
  b [VLDMDIA_UPD_d8_15, TPOP2_r4_7, TPOP2_r4_7_RET]

isInstr i o = (TargetInstruction i) `elem` oInstructions o

isTerm o = isBranch o || isTailCall o

-- Activate the SP adjustment operations if there are non-fixed stack objects or
--  SP-relative stores (typically to store function call arguments).
enforceStackFrame f @ Function {fCode = code, fStackFrame = frame} =
  let fcode = flatten code
      pcg   = P.fromGraph $ CG.fromFunction f
      t2rs  = M.fromListWith (++) [(undoPreAssign t, maybeToList $ tReg t)
                                  | t <- tOps fcode]
  in if length frame > 0 || any (isStackAccess (pcg, t2rs)) fcode
     then mapToOperation activateSPAdjusts f
     else f

isStackAccess aux
  SingleOperation {oOpr = Natural Linear {oIs = is, oUs = _:addr:_}}
  | any (\i -> TargetInstruction i `elem` is) [T2STRi12, T2STRBi12, VSTRS] =
    isSP aux addr
isStackAccess _ _ = False

-- if the temporaries in the addr operand are in the same CG partition as a
-- temporary that is potentially preassigned to SP
isSP (pcg, t2rs) addr =
  let -- enough with a representative as they are all in the same CG partition
      at   = head $ map undoPreAssign $ extractTemps addr
      cgts = map mkTemp $ fromJust $ P.equivalent pcg at
      sp   = or [any isSPRegister (t2rs M.! t) | t <- cgts]
  in sp

isSPRegister Register {regId = TargetRegister SP} = True
isSPRegister _ = False

activateSPAdjusts
  o @ SingleOperation {oOpr = Natural spo @ Linear {
                          oIs = [General NullInstruction,
                                 TargetInstruction i]}}
  | isSPAdjustInstr i =
    let o' = o {oOpr = Natural spo {oIs = [TargetInstruction i]}}
    in mapToActivators (const []) o'
activateSPAdjusts o = o

isSPAdjustInstr i = i `elem` [TSUBspi_pseudo, TADDspi_pseudo]
