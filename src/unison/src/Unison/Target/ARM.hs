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
module Unison.Target.ARM (target) where

import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Control.Arrow

import Common.Util

import MachineIR hiding (parse)
import MachineIR.Transformations.AddImplicitRegs

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Target.Query
import Unison.Analysis.TemporaryType
import Unison.Transformations.FoldReservedRegisters
import Unison.Analysis.TransitiveOperations

import Unison.Target.ARM.Common
import Unison.Target.ARM.Registers
import Unison.Target.ARM.OperandInfo
import Unison.Target.ARM.Transforms
import Unison.Target.ARM.Usages
import Unison.Target.ARM.ARMRegisterDecl
import Unison.Target.ARM.ARMRegisterClassDecl
import Unison.Target.ARM.ARMResourceDecl
import Unison.Target.ARM.SpecsGen.ARMInstructionDecl
import Unison.Target.ARM.SpecsGen.ARMItineraryDecl
import qualified Unison.Target.ARM.SpecsGen as SpecsGen

target =
    API.TargetDescription {
      API.tRegisterArray    = const registerArray,
      API.tRegisterAtoms    = const registerAtoms,
      API.tRegClasses       = const regClasses,
      API.tRegisters        = const registers,
      API.tInfRegClassUsage = const infRegClassUsage,
      API.tInfRegClassBound = const infRegClassBound,
      API.tSubRegIndexType  = const subRegIndexType,
      API.tCallerSaved      = const callerSaved,
      API.tCalleeSaved      = const calleeSaved,
      API.tReserved         = const reserved,
      API.tInstructionType  = const instructionType,
      API.tBranchInfo       = const branchInfo,
      API.tPreProcess       = preProcess,
      API.tPostProcess      = postProcess,
      API.tTransforms       = const transforms,
      API.tCopies           = const copies,
      API.tRematInstrs      = const rematInstrs,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = const operandInfo,
      API.tAlignedPairs     = const SpecsGen.alignedPairs,
      API.tPackedPairs      = const (const (const [])),
      API.tRelatedPairs     = const (const []),
      API.tResources        = const resources,
      API.tUsages           = usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy,
      API.tConstraints      = const constraints
    }

instance Read ARMInstruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

-- | Gives the type of natural operation according to the instruction

instructionType i
    | i `elem` [TCRETURNdi, TCRETURNri, TTAILJMPdND, TTAILJMPr] =
      TailCallInstructionType
    | otherwise = SpecsGen.instructionType i

-- | Gives the target of a jump instruction and the type of jump

branchInfo (Branch {oBranchIs = is, oBranchUs = [BlockRef l, _, _]})
  | targetInst is `elem` [T2Bcc, TBcc] =
    BranchInfo Conditional (Just l)

branchInfo (Branch {oBranchIs = is, oBranchUs = [_, BlockRef l]})
  | targetInst is `elem` [TCBNZ, TCBZ] =
    BranchInfo Conditional (Just l)

branchInfo (Branch {oBranchIs = is, oBranchUs = [BlockRef l, _, _]})
  | targetInst is `elem` [T2B, TB] =
    BranchInfo Unconditional (Just l)

branchInfo (Branch {oBranchIs = is})
  | (targetInst is `elem`
     [TBX_RET, T2LDMIA_RET, TPOP_RET, T2BR_JT, T2TBB_JT, T2TBH_JT]) ||
    (SpecsGen.parent (targetInst is) `elem` [Just T2LDMIA_RET, Just TPOP_RET]) =
    BranchInfo Unconditional Nothing

branchInfo o = error ("unmatched pattern: branchInfo " ++ show (mkSingleOperation (-1) (Natural o)))

-- | Gives a set of def copies and a list of sets of use copies to extend the
-- given temporary

-- Do not extend temporaries that are defined by virtual defines and used once
copies _fInfo False _t _rs d [_] | isDefine d = ([], [[]])

-- Do not extend temporaries that are only used by virtual kills
copies _fInfo False _t _rs _d [u] | isKill u = ([], [[]])

-- Do not extend temporaries pre-assigned to reserved registers
copies _ False _ rs _ us | any isReserved rs =
  ([], replicate (length us) [])

-- Add only one store for entry callee-saved temporaries
-- Add only one load for exit callee-saved temporaries
-- Do not add copies for intermediate callee-saved temporaries
copies (f, cst, _, _, _, _) False t rs _ [_] | not (null rs) && S.member t cst =
      (if isEntryTemp (fCode f) t
       then [mkNullInstruction, TargetInstruction (pushInstruction rs)]
       else [],
       [if isExitTemp (fCode f) t
        then [mkNullInstruction, TargetInstruction (popInstruction rs)]
        else []])

-- Do not extend non-pre-allocated temporaries that are only "passed through" a
-- block without calls
copies (f, _, _, _, _, _) False t [] d [u]
    | isIn d && isOut u && not (any isCall (bCode $ tempBlock (fCode f) t)) =
    ([], [[]])

-- Do not extend rematerializable instructions used only once, locally
-- FIXME: review whether this is always safe
copies (Function {fCode = code}, _, _, _, _, _) False t _ d [u]
  | isNatural d && (isNatural u || isFun u) &&
    (isRematerializable (targetInst (oInstructions d))) &&
    not (mayCrossMemDep readWriteInfo d u code) &&
    compatibleClassesForTemp t [d, u] = ([], [[]])

copies (f, _, cg, ra, bcfg, sg) _ t _rs d us =
  let is     = d:us
      w      = widthOfTemp ra cg f t is
      dors   = transitivePreAssignments bcfg sg Reaching f t
      uors   = transitivePreAssignments bcfg sg Reachable f t
      size   = any ((==) Size) $ fGoal f
  in (
      (defCopies size w dors),
      map (const (useCopies size w uors)) us
      )

pushInstruction [r]
  | r == R4_7  = TPUSH_r4_7
  | r == R8_11 = TPUSH_r8_11
  | r == D8_15 = VSTMDDB_UPD_d8_15

popInstruction [r]
  | r == R4_7  = TPOP_r4_7
  | r == R8_11 = TPOP_r8_11
  | r == D8_15 = VLDMDIA_UPD_d8_15

defCopies _ _ [Register (TargetRegister R7)] = []
defCopies size w _ =
  [mkNullInstruction] ++
   map TargetInstruction (moveInstrs size w) ++
   map TargetInstruction (storeInstrs size w)

useCopies _ _ [Register (TargetRegister R7)] = []
useCopies size w _ =
  [mkNullInstruction] ++
   map TargetInstruction (moveInstrs size w) ++
   map TargetInstruction (loadInstrs size w)

classOfTemp = classOf (target, [])
widthOfTemp = widthOf (target, [])

compatibleClassesForTemp t os =
  let regs = [S.fromList $ registers $ fromJust (classOfTemp t o) | o <- os]
  in not $ S.null $ foldl S.intersection (head regs) regs

-- TODO: add STORE_S, LOAD_S, also GPR <-> SPR moves?

-- TMOVr, {TMOVr, VMOVS, VMOVSR, VMOVRS} (MOVE_ALL is instanciated after
-- register allocation since their properties are all the same -- except TMOVr
-- has size 2)
moveInstrs size 1
  | size = [MOVE, MOVE_ALL]
  | otherwise = [MOVE_ALL]

-- VMOVD
moveInstrs _ 2 = [MOVE_D]

-- {T2STRi12, tSTRi}
storeInstrs size 1
  | size = [STORE, STORE_T]
  | otherwise = [STORE]

-- VSTRD
storeInstrs _ 2 = [STORE_D]

-- {T2LDRi12, tLDRi}
loadInstrs size 1
  | size = [LOAD, LOAD_T]
  | otherwise = [LOAD]

-- VLDRD
loadInstrs _ 2 = [LOAD_D]

isReserved r = r `elem` reserved

rematInstrs i
  | isRematerializable i =
      Just (sourceInstr i, dematInstr i, rematInstr i)
  | otherwise = error ("unmatched: rematInstrs " ++ show i)

-- | Transforms copy instructions into natural instructions

-- handle regular copies
fromCopy _ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MOVE, MOVE_ALL, MOVE_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs = [s] ++ defaultUniPred,
            oDs = [d]}
  | i `elem` [STORE, STORE_T, STORE_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs  = [mkOprArmSP, mkBoundMachineFrameObject i d, s] ++
                   defaultUniPred,
            oDs  = []}
  | i `elem` [LOAD, LOAD_T, LOAD_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs  = [mkOprArmSP, mkBoundMachineFrameObject i s] ++
                   defaultUniPred,
            oDs  = [d]}
  | i `elem` [TPUSH2_r4_7, TPUSH2_r4_11] =
    let w = i == TPUSH2_r4_11
    in Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
               oUs = [mkOprArmSP | w] ++ defaultUniPred ++
                     map (Register . TargetRegister) (pushRegs i ++ [LR]),
               oDs = [mkOprArmSP | w]}
  | i `elem` [VSTMDDB_UPD_d8_15] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs = [mkOprArmSP] ++ defaultUniPred ++ mkPushRegs i,
            oDs = [mkOprArmSP]}
  | i `elem` [TPOP2_r4_7, TPOP2_r4_7_RET, TPOP2_r4_11, TPOP2_r4_11_RET] =
    let w = i `elem` [TPOP2_r4_11, TPOP2_r4_11_RET]
    in Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
               oUs = [mkOprArmSP | w] ++ defaultUniPred ++ mkPushRegs i,
               oDs = [mkOprArmSP | w]}
  | i `elem` [VLDMDIA_UPD_d8_15] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs = [mkOprArmSP] ++ defaultUniPred ++ mkPushRegs i,
            oDs = [mkOprArmSP]}

-- handle rematerialization copies
fromCopy (Just (Linear {oUs = us}))
         Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | isDematInstr i =
    Linear {oIs = [mkNullInstruction], oUs = [s], oDs = [d]}
  | isRematInstr i =
    Linear {oIs = [TargetInstruction (originalInstr i)], oUs = us, oDs = [d]}

-- handle rematerialization sources
fromCopy _ (Natural o @ Linear {oIs = [TargetInstruction i]})
  | isSourceInstr i = o {oIs = [mkNullInstruction]}

fromCopy _ (Natural o) = o
fromCopy _ o = error ("unmatched pattern: fromCopy " ++ show o)

mkPushRegs i = map (Register . TargetRegister) (pushRegs i)

mkOprArmSP = Register $ mkTargetRegister SP

mkBoundMachineFrameObject i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size False)

stackSize i
  | i `elem` [STORE, STORE_T, LOAD, LOAD_T] = 1
  | i `elem` [STORE_D, LOAD_D] = 2

fromCopyInstr MOVE     _ = TMOVr
fromCopyInstr MOVE_D   _ = VMOVD
fromCopyInstr STORE    _ = T2STRi12
fromCopyInstr STORE_T  _ = TSTRi
fromCopyInstr STORE_D  _ = VSTRD
fromCopyInstr LOAD     _ = T2LDRi12
fromCopyInstr LOAD_T   _ = TLDRi
fromCopyInstr LOAD_D   _ = VLDRD
fromCopyInstr MOVE_ALL (s, d)
  | isGPR s && isGPR d = TMOVr
  | isGPR s && isSPR d = VMOVSR
  | isSPR s && isGPR d = VMOVRS
  | isSPR s && isSPR d = VMOVS
fromCopyInstr TPUSH2_r4_7 _  = TPUSH
fromCopyInstr TPUSH2_r4_11 _ = T2STMDB_UPD
fromCopyInstr VSTMDDB_UPD_d8_15 _ = VSTMDDB_UPD
fromCopyInstr TPOP2_r4_7 _      = TPOP
fromCopyInstr TPOP2_r4_7_RET _  = TPOP_RET
fromCopyInstr TPOP2_r4_11 _     = T2LDMIA_UPD
fromCopyInstr TPOP2_r4_11_RET _ = T2LDMIA_RET
fromCopyInstr VLDMDIA_UPD_d8_15 _ = VLDMDIA_UPD

isSPR r = rTargetReg (regId r) `elem` registers (RegisterClass SPR)
isGPR r = rTargetReg (regId r) `elem` registers (RegisterClass GPR)

-- | Declares target architecture resources

resources =
    [

     -- Boundle width (times 16 bits): upper bound given by size of compound
     -- instructions to be expanded

     Resource BundleWidth 5,

     -- Resources as defined by ARMScheduleV6

     Resource V6_Pipe 1

    ]

-- | No-operation instruction

nop = Linear [TargetInstruction NOP] [] []

readWriteInfo i
  -- copies do not have memory side effects (loads and stores do not alias
  -- with other memory accesses as they operate on spill slots only)
  | SpecsGen.instructionType i == CopyInstructionType = SpecsGen.readWriteInfo i
  -- complete memory side effects (some mayLoad/mayStore info is missing)
  | SpecsGen.itinerary i `elem`
    [IIC_iLoad_m, IIC_iLoad_mu, IIC_iLoad_mBr, IIC_iLoad_bh_ru, IIC_iLoad_bh_iu,
     IIC_iLoad_bh_r, IIC_iLoad_bh_si, IIC_iLoad_d_r, IIC_iLoad_d_ru,
     IIC_iLoad_i, IIC_iLoadiALU, IIC_iLoad_ru, IIC_iLoad_iu, IIC_iLoad_r,
     IIC_iLoad_si, IIC_fpLoad_mu, IIC_fpLoad_m, IIC_fpLoad64, IIC_fpLoad32,
     IIC_iLoad_bh_i, IIC_iLoad_d_i] =
      first addMem $ SpecsGen.readWriteInfo i
  | SpecsGen.itinerary i `elem`
    [IIC_iStore_r, IIC_iStore_bh_r, IIC_iStore_m, IIC_iStore_mu,
     IIC_iStore_bh_ru, IIC_iStore_bh_iu, IIC_iStore_ru, IIC_iStore_bh_si,
     IIC_iStore_d_r, IIC_iStore_d_ru, IIC_iStore_iu, IIC_iStore_si,
     IIC_fpStore_mu, IIC_fpStore_m, IIC_fpStore64, IIC_fpStore32,
     IIC_iStore_bh_i, IIC_iStore_i] =
      second addMem $ SpecsGen.readWriteInfo i
  | i `elem` [TSUBspi_pseudo, TADDspi_pseudo] =
      second (++ [OtherSideEffect SP]) $ SpecsGen.readWriteInfo i
  | otherwise = SpecsGen.readWriteInfo i

addMem = (++ [Memory "mem"])

-- | Implementation of frame setup and destroy operations. All functions
-- observed so far have a reserved call frame (hasReservedCallFrame(MF)), which
-- means frame setup and destroy operations are just removed (see
-- ARMFrameLowering.cpp ("eliminateCallFramePseudoInstr")).

implementFrame = const []

-- | Adds function prologue, see corresponding logic in ARMFrameLowering.cpp
-- ("emitPrologue")

-- We need a stack frame iff there are 1) spills or 2) non-fixed stack objects
-- or SP-relative stores. This takes care of 1), the transformation
-- 'enforceStackFrame' at AugmentPostRW takes care of 2) which is a static
-- condition. Additionally, we need to adjust the SP by 1 if we have an odd
-- number of callee-saved spills for alignment reasons (see
-- http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka4127.html).
-- This is not yet supported, instead, 'uni normalize' removes SP adjustments in
-- such cases to ensure a fair comparison to LLVM.

addPrologue (_, oid, _) (e:code) =
  let subSp = mkAct $ mkOpt oid TSUBspi_pseudo [Bound mkMachineFrameSize] []
  in case split (keepDelimsR $ whenElt isFPPush) code of
      [before, after] -> [e] ++ before ++ [subSp] ++ after
      _ -> case split (dropInitBlank $ condense $ whenElt isStoreCopy) code of
            before : after -> [e] ++ before ++ [subSp] ++ concat after

isFPPush o = TargetInstruction (VSTMDDB_UPD_d8_15) `elem` oInstructions o
isStoreCopy o = any (\i -> TargetInstruction i `elem` oInstructions o)
                [STORE, STORE_D, TPUSH2_r4_7, VSTMDDB_UPD_d8_15]

addEpilogue (_, oid, _) code =
  let addSp = mkAct $ mkOpt oid TADDspi_pseudo [Bound mkMachineFrameSize] []
  in case split (keepDelimsL $
                 whenElt (\o -> isPopRet o || isBranch o || isTailCall o))
          code of
      f : e -> f ++ [addSp] ++ concat e
      os    -> error ("unhandled epilogue: " ++ show os)

isPopRet o = any (\i -> TargetInstruction i `elem` oInstructions o)
             [TPOP2_r4_7_RET, VLDMDIA_UPD_d8_15]

mkOpt oid inst us ds =
  makeOptional $ mkLinear oid [TargetInstruction inst] us ds

mkAct = addActivators (map TargetInstruction spillInstrs)

addActivators = mapToActivators . (++)

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess to = [mapToTargetMachineInstruction instantiateMEMCPY,
                 cleanConstPoolBlocks,
                 mapToMachineInstruction promoteImplicitOperands,
                 mapToMachineInstruction hideCPSRRegister,
                 mapToMachineInstruction explicateRedefPatterns,
                 mapToTargetMachineInstruction addFrameIndex,
                 mapToTargetMachineInstruction processTailCalls,
                 mapToTargetMachineInstruction collapseVarOpInstructions,
                 if align to then id else relaxAlignment]

instantiateMEMCPY
  mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                      msOperands = mos}
  | i == MEMCPY = case miValue (mos !! 4) of
    4 -> mi {msOpcode   = mkMachineTargetOpc MEMCPY_4,
             msOperands = slice 0 1 mos ++ slice 5 8 mos ++ slice 2 4 mos}
    n -> error ("FIXME: implement MEMCPY_" ++ show n)
instantiateMEMCPY mi = mi

cleanConstPoolBlocks mf @ MachineFunction {mfBlocks = mbs} =
  mf {mfBlocks = filter (not . isConstPoolBlock) mbs}

isConstPoolBlock MachineBlock {mbInstructions = mis} =
  (not $ null mis) &&
  all (\mi ->
         (isMachineTarget mi && mopcTarget (msOpcode mi) == CONSTPOOL_ENTRY) ||
         (isMachineVirtual mi && mopcVirtual (msOpcode mi) == EXIT)) mis

promoteImplicitOperands
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [o1, o2, o3, p1, p2,
                                    cc @ MachineReg {mrName = CPSR}]}
  | i `elem` [T2SUBrr, T2SUBri, T2ORRrr, T2ANDri, T2ADDri] =
    let mos' = [o1, cc, o2, o3, p1, p2]
    in mi {msOpcode = mkMachineTargetOpc (toExplicitCpsrDef i),
           msOperands = mos'}

promoteImplicitOperands
  mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = mos} =
    let fu   = length $ snd $ operandInfo i
        mos' = if writesSideEffect i CPSR
               then insertAt (mkMachineReg CPSR) (fu - 1) mos
               else mos
    in mi {msOperands = mos'}

promoteImplicitOperands mi = mi

writesSideEffect i eff =
    (OtherSideEffect eff) `elem` (snd $ SpecsGen.readWriteInfo i)

-- This is done to prevent 'extractCallRegs' adding the 'cpsr' register as
-- argument to function calls

hideCPSRRegister mi @ MachineSingle {msOperands = mos} =
  let mos' = mapIf isMachineReg hideCPSR mos
  in mi {msOperands = mos'}

hideCPSR mr @ MachineReg {mrName = CPSR} = mr {mrName = PRED}
hideCPSR mr = mr

explicateRedefPatterns
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [d, u1, u2, u3, p1, p2, s,
                                    MachineTemp {mtId = tid,
                                                 mtFlags = [MachineRegImplicit],
                                                 mtTiedDef = Just 0}]}
  | isRedefableInstr i =
    let i'   = mkMachineTargetOpc (redefInstr i)
        mos' = [d, u1, u2, u3, mkSimpleMachineTemp tid, p1, p2, s]
    in mi {msOpcode = i', msOperands = mos'}

explicateRedefPatterns
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [d, u1, u2, p1, p2, s,
                                    MachineTemp {mtId = tid,
                                                 mtFlags = [MachineRegImplicit],
                                                 mtTiedDef = Just 0}]}
  | isRedefableInstr i =
    let i'   = mkMachineTargetOpc (redefInstr i)
        mos' = [d, u1, u2, mkSimpleMachineTemp tid, p1, p2, s]
    in mi {msOpcode = i', msOperands = mos'}

explicateRedefPatterns mi = mi


liftToTOpc f = mkMachineTargetOpc . f . mopcTarget

addFrameIndex mi @ MachineSingle {msOpcode = opcode,
                                  msOperands = operands}
  | any isMachineConstantPoolIndex operands &&
    any isTemporaryInfo (fst $ operandInfo $ mopcTarget opcode) =
    mi {msOpcode = liftToTOpc (\i -> read (show i ++ "_cpi")) opcode}

addFrameIndex mi @ MachineSingle {msOpcode = opcode,
                                  msOperands = operands}
  | any isMachineFrameIndex operands &&
    any isTemporaryInfo (fst $ operandInfo $ mopcTarget opcode) =
      mi {msOpcode = liftToTOpc (\i -> read (show i ++ "_fi")) opcode}
  | otherwise = mi

processTailCalls mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                     msOperands = s:mos}
  | i == TCRETURNdi =
    let mos' = [s] ++ defaultMIRPred ++ mos
    in mi {msOpcode = mkMachineTargetOpc TTAILJMPdND, msOperands = mos'}
  | i == TCRETURNri = mi {msOpcode = mkMachineTargetOpc TTAILJMPr}

processTailCalls mi = mi

collapseVarOpInstructions mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                              msOperands = p1:p2:mos}
  | i `elem` [TPUSH, TPOP_RET] =
    let i' = varOpPseudo i mos
    in mi {msOpcode = mkMachineTargetOpc i', msOperands = [p1,p2]}

collapseVarOpInstructions mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                              msOperands = sp:sp':p1:p2:mos}
  | i `elem` [T2STMDB_UPD, T2LDMIA_UPD, T2LDMIA_RET] =
    let i' = varOpPseudo i mos
    in mi {msOpcode = mkMachineTargetOpc i', msOperands = [sp,sp',p1,p2]}
collapseVarOpInstructions mi = mi

varOpPseudo TPUSH mos
  | any (isMachineRegWith R7)  mos = TPUSH_4_7
  | any (isMachineRegWith R6)  mos = TPUSH_4_6
  | any (isMachineRegWith R5)  mos = TPUSH_4_5
  | any (isMachineRegWith R4)  mos = TPUSH_4
varOpPseudo T2STMDB_UPD mos
  | any (isMachineRegWith R11) mos = T2STMDB_UPD_4_11
  | any (isMachineRegWith R10) mos = T2STMDB_UPD_4_10
  | any (isMachineRegWith R9)  mos = T2STMDB_UPD_4_9
  | any (isMachineRegWith R8)  mos = T2STMDB_UPD_4_8
varOpPseudo TPOP_RET mos
  | any (isMachineRegWith R7)  mos = TPOP_RET_4_7
  | any (isMachineRegWith R6)  mos = TPOP_RET_4_6
  | any (isMachineRegWith R5)  mos = TPOP_RET_4_5
  | any (isMachineRegWith R4)  mos = TPOP_RET_4
varOpPseudo T2LDMIA_UPD mos
  | any (isMachineRegWith R11) mos = T2LDMIA_UPD_4_11
  | any (isMachineRegWith R10) mos = T2LDMIA_UPD_4_10
  | any (isMachineRegWith R9)  mos = T2LDMIA_UPD_4_9
  | any (isMachineRegWith R8)  mos = T2LDMIA_UPD_4_8
  | any (isMachineRegWith R7)  mos = T2LDMIA_UPD_4_7
  | any (isMachineRegWith R6)  mos = T2LDMIA_UPD_4_6
  | any (isMachineRegWith R5)  mos = T2LDMIA_UPD_4_5
  | any (isMachineRegWith R4)  mos = T2LDMIA_UPD_4_4
varOpPseudo T2LDMIA_RET mos
  | any (isMachineRegWith R11) mos = T2LDMIA_RET_4_11
  | any (isMachineRegWith R10) mos = T2LDMIA_RET_4_10
  | any (isMachineRegWith R9)  mos = T2LDMIA_RET_4_9
  | any (isMachineRegWith R8)  mos = T2LDMIA_RET_4_8

isMachineRegWith r MachineReg {mrName = r'} = r == r'
isMachineRegWith _ _ = False

relaxAlignment mf @ MachineFunction {mfProperties = mps} =
  -- if the only (non-fixed) objects in the stack are callee-saved registers and
  -- there are no SP-relative stores, the only reason for SP adjustments is the
  -- alignment constraints
  -- (http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka4127.html).
  -- In that case, remove them.
  let regularObjects = case find isMachineFunctionPropertyFrame mps of
        Just (MachineFunctionPropertyFrame ff)
          | all isCSRegisterObject ff -> False
        Nothing -> False
        _ -> True
      spStores = any isSPStore (flattenMachineFunction mf)
  in if regularObjects || spStores then mf else cleanSPAdjusts mf

isSPStore MachineSingle {msOpcode   = MachineTargetOpc i,
                         msOperands = mos}
  | i `elem` [T2STRBi12, T2STRDi8, T2STRi12, TSTRspi, VSTRS]
    && any isSPRegister mos = True
isSPStore _ = False

isSPRegister MachineReg {mrName = SP} = True
isSPRegister _ = False

cleanSPAdjusts = filterMachineInstructions (not . isSPAlign)

isSPAlign MachineSingle {msOpcode   = MachineTargetOpc i,
                         msOperands = [_, _, MachineImm 1, _, _]}
  | i `elem` [TSUBspi, TADDspi] = True
isSPAlign _ = False

isCSRegisterObject MachineFrameObjectInfo {mfoiCSRegister = (Just _)} = True
isCSRegisterObject _ = False

-- | Target dependent post-processing functions

postProcess to = [expandPseudos to, removeAllNops, removeFrameIndex,
                  cleanLoadMerges,
                  removeEmptyBundles, reorderImplicitOperands,
                  exposeCPSRRegister,
                  mapToTargetMachineInstruction expandVarOpInstructions,
                  flip addImplicitRegs (target, []),
                  demoteImplicitOperands]

expandPseudos to = mapToMachineBlock (expandBlockPseudos (expandPseudo to))

expandPseudo to mi @ MachineSingle {
  msOpcode   = MachineTargetOpc T2MOVi32imm,
  msOperands = [dst, ga @ MachineGlobalAddress {}]}
  | not (unitLatency to) =
    let mi1 = mi {msOpcode   = mkMachineTargetOpc T2MOVi16,
                  msOperands = [dst, ga] ++ defaultMIRPred}
        mi2 = mi {msOpcode   = mkMachineTargetOpc T2MOVTi16,
                  msOperands = [dst, dst, ga] ++ defaultMIRPred}
    in [[mi1], [mi2]]

expandPseudo to mi @ MachineSingle {
  msOpcode   = MachineTargetOpc i,
  msOperands = [d, u1, u2, u3, _, cc, p, s]}
  | not (unitLatency to) && isRedefInstr i =
    let [ci, ri] = ccInstrs i
        mi1 = mi {msOpcode = mkMachineTargetOpc ci,
                  msOperands = [cc, mkMachineImm 8]}
        mi2 = mi {msOpcode = mkMachineTargetOpc ri,
                  msOperands = [d, u1, u2, u3, cc, p, s]}
    in [[mi1], [mi2]]

expandPseudo to mi @ MachineSingle {
  msOpcode   = MachineTargetOpc i,
  msOperands = [d, u1, u2, _, cc, p, s]}
  | not (unitLatency to) && isRedefInstr i =
    let [ci, ri] = ccInstrs i
        mi1 = mi {msOpcode = mkMachineTargetOpc ci,
                  msOperands = [cc, mkMachineImm 8]}
        mi2 = mi {msOpcode = mkMachineTargetOpc ri,
                  msOperands = [d, u1, u2, cc, p, s]}
    in [[mi1], [mi2]]

expandPseudo to mi @ MachineSingle {
  msOpcode   = MachineTargetOpc i,
  msOperands = [_, u1, u2, cc, p]}
  | not (unitLatency to) && i `elem` condMoveInstrs =
    let [ci, ri] = ccInstrs i
        mi1 = mi {msOpcode   = mkMachineTargetOpc ci,
                  -- FIXME: compute mask correctly
                  msOperands = [cc, mkMachineImm 8]}
        mi2 = mi {msOpcode   = mkMachineTargetOpc ri,
                  msOperands = [u1, u2, cc, p]}
    in [[mi1], [mi2]]

-- TODO: expand 'T2MOVCCi32imm'

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc TFP} =
  [[mi {msOpcode = mkMachineTargetOpc TADDrSPi}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                   msOperands = [off]}
  | i `elem` [TSUBspi_pseudo, TADDspi_pseudo] =
    let i' = case i of
              TSUBspi_pseudo -> TSUBspi
              TADDspi_pseudo -> TADDspi
        sp = mkMachineReg SP
    in [[mi {msOpcode = mkMachineTargetOpc i',
             msOperands = [sp, sp, off] ++ defaultMIRPred}]]

expandPseudo _ mi = [[mi]]

pushRegs i
  | i `elem` [TPUSH2_r4_7, TPOP2_r4_7, TPOP2_r4_7_RET] =
      [R4, R5, R6, R7]
  | i `elem` [TPUSH2_r4_11, TPOP2_r4_11, TPOP2_r4_11_RET] =
      pushRegs TPUSH2_r4_7 ++ [R8, R9, R10, R11]
  | i `elem` [VSTMDDB_UPD_d8_15, VLDMDIA_UPD_d8_15] =
      [D8, D9, D10, D11, D12, D13, D14, D15]
pushRegs i = error ("unmatched: pushRegs " ++ show i)

removeAllNops =
  filterMachineInstructions
  (\mi -> not (isMachineTarget mi && mopcTarget (msOpcode mi) == NOP))

reorderImplicitOperands = mapToMachineInstruction reorderImplicitOperandsInInstr

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = MachineReg {mrName = CPSR} : _}
  | i `elem` [T2TSTri_cpsr, T2CMNri_cpsr, T2CMPrr_cpsr, T2TSTrr_cpsr,
              T2SUBrr_cpsr, TCMPi8_cpsr, FMSTAT_cpsr] =
      mi {msOpcode = mkMachineTargetOpc $ fromExplicitCpsrDef i}

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | i `elem` [T2CMPri_cpsr] =
      mi {msOpcode = mkMachineTargetOpc $ fromExplicitCpsrDef i}

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [d, u1, p1, p2, _]}
  | i == TMOVi8s =
    let mos' = [d, mkMachineReg CPSR, u1, p1, p2]
    in mi {msOpcode = mkMachineTargetOpc TMOVi8, msOperands = mos'}

reorderImplicitOperandsInInstr mi = mi

exposeCPSRRegister = mapToMachineInstruction exposeCPSRRegisterInInstr

exposeCPSRRegisterInInstr mi @ MachineSingle {msOperands = mos} =
  let mos' = mapIf isMachineReg exposeCPSR mos
  in mi {msOperands = mos'}

exposeCPSR mr @ MachineReg {mrName = PRED} = mr {mrName = CPSR}
exposeCPSR mr = mr

removeFrameIndex = mapToMachineInstruction removeFrameIndexInstr

removeFrameIndexInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                      msOperands = [d, off, MachineImm {miValue = 0}, cc, p]}
  | i `elem` [T2LDRi12_fi, VLDRD_fi] =
    let mos = [d, mkMachineReg SP, off, cc, p]
    in mi {msOpcode = mkMachineTargetOpc $ removeFi i, msOperands = mos}

removeFrameIndexInstr mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | "_fi" `isSuffixOf`  (show i) =
    mi {msOpcode = mkMachineTargetOpc $ removeFi i}
  | "_cpi" `isSuffixOf` (show i) =
    mi {msOpcode = mkMachineTargetOpc $ removeCpi i}
  | otherwise = mi

removeFi i = read $ dropSuffix "_fi" (show i)
removeCpi i = read $ dropSuffix "_cpi" (show i)

cleanLoadMerges = filterMachineInstructions (not . isSingleLoadMerge)

isSingleLoadMerge MachineSingle {msOpcode = MachineTargetOpc Load_merge} = True
isSingleLoadMerge _ = False

removeEmptyBundles = filterMachineInstructions (const True)

-- This is the inverse of 'promoteImplicitOperands'. TODO: does this preclude
-- 'reorderImplicitOperands'?

demoteImplicitOperands = mapToMachineInstruction demoteImplicitOperandsInInstr

demoteImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = mos,
                      msProperties = ps} =
    let (mos', ps') =
          if writesSideEffect i CPSR
          then let (ds, us) = splitAt (nDefs i) mos
                   ds' = filter (not . isCPSROrNullReg) ds
               in (ds' ++ us,
                   ps ++ [mkMachineInstructionPropertyDefs
                          (toInteger $ length ds')])
          else (mos, ps)
    in mi {msOperands = mos', msProperties = ps'}

nDefs = length . snd . operandInfo

isCPSROrNullReg mo = isMachineCPSRReg mo || isMachineNullReg mo

isMachineCPSRReg (MachineReg {mrName = CPSR}) = True
isMachineCPSRReg _ = False

-- | This is the inverse of 'collapseVarOpInstructions'

expandVarOpInstructions mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                            msOperands = mos}
  | SpecsGen.parent i `elem` [Just TPUSH, Just T2STMDB_UPD, Just T2LDMIA_UPD] =
    let i'   = fromJust $ SpecsGen.parent i
        mos' = mos ++ map mkMachineReg (varOps i ++ [LR])
    in mi {msOpcode = mkMachineTargetOpc i', msOperands = mos'}
  | SpecsGen.parent i `elem` [Just TPOP_RET, Just T2LDMIA_RET] =
    let i'   = fromJust $ SpecsGen.parent i
        mos' = mos ++ map mkMachineReg (varOps i ++ [PC])
    in mi {msOpcode = mkMachineTargetOpc i', msOperands = mos'}
expandVarOpInstructions mi = mi

varOps TPUSH_4   = [R4]
varOps TPUSH_4_5 = [R4, R5]
varOps TPUSH_4_6 = [R4, R5, R6]
varOps TPUSH_4_7 = [R4, R5, R6, R7]

varOps T2STMDB_UPD_4_8  = [R4, R5, R6, R7, R8]
varOps T2STMDB_UPD_4_9  = [R4, R5, R6, R7, R8, R9]
varOps T2STMDB_UPD_4_10 = [R4, R5, R6, R7, R8, R9, R10]
varOps T2STMDB_UPD_4_11 = [R4, R5, R6, R7, R8, R9, R10, R11]

varOps TPOP_RET_4   = [R4]
varOps TPOP_RET_4_5 = [R4, R5]
varOps TPOP_RET_4_6 = [R4, R5, R6]
varOps TPOP_RET_4_7 = [R4, R5, R6, R7]

varOps T2LDMIA_UPD_4_4  = [R4]
varOps T2LDMIA_UPD_4_5  = [R4, R5]
varOps T2LDMIA_UPD_4_6  = [R4, R5, R6]
varOps T2LDMIA_UPD_4_7  = [R4, R5, R6, R7]
varOps T2LDMIA_UPD_4_8  = [R4, R5, R6, R7, R8]
varOps T2LDMIA_UPD_4_9  = [R4, R5, R6, R7, R8, R9]
varOps T2LDMIA_UPD_4_10 = [R4, R5, R6, R7, R8, R9, R10]
varOps T2LDMIA_UPD_4_11 = [R4, R5, R6, R7, R8, R9, R10, R11]

varOps T2LDMIA_RET_4_8  = [R4, R5, R6, R7, R8]
varOps T2LDMIA_RET_4_9  = [R4, R5, R6, R7, R8, R9]
varOps T2LDMIA_RET_4_10 = [R4, R5, R6, R7, R8, R9, R10]
varOps T2LDMIA_RET_4_11 = [R4, R5, R6, R7, R8, R9, R10, R11]

-- | Gives a list of function transformers
transforms ImportPreLift = [peephole extractReturnRegs,
                            (\f -> foldReservedRegisters f (target, [])),
                            mapToOperationWithGoals addThumbAlternatives,
                            peephole expandMEMCPY]
transforms ImportPostLift = [peephole handlePromotedOperands,
                             defineFP]
transforms AugmentPreRW = [peephole combinePushPops,
                           peephole expandRets,
                           fixpoint (peephole normalizeLoadStores),
                           peephole combineLoadStores,
                           reorderCalleeSavedSpills]

transforms AugmentPostRW = [enforceStackFrame]

transforms _ = []

mapToOperationWithGoals t f @ Function {fCode = code, fGoal = gs} =
  f {fCode = map (mapToOperationInBlock (t gs)) code}

-- | Latency of read-write dependencies

readWriteLatency _ _ (_, Read) (_, Write) = 0
readWriteLatency _ _ ((_, VirtualType (DelimiterType InType)), _) (_, _) = 1
readWriteLatency _ _ ((_, VirtualType FunType), _) (_, _) = 1
readWriteLatency _ _ ((_, VirtualType _), _) (_, _) = 0
readWriteLatency to _ ((TargetInstruction p, _), _) (_, _) =
    maybeMax 0 $ map occupation (usages to p)


-- | Alternative temporaries of each operand

-- All temps that hold the same value

alternativeTemps _ _ _ ts = map fst ts

-- | Copy expansion

expandCopy _ _ o = [o]

-- | Custom processor constraints

constraints f =
  foldMatch altRetConstraints [] f ++
  foldMatch altLoadStoreConstraints [] f

altRetConstraints (
  op @ SingleOperation {oOpr = Copy {
       oCopyIs = [General NullInstruction,
                  TargetInstruction TPOP2_r4_7_RET,
                  TargetInstruction TPOP2_r4_11_RET]}}
  :
  or @ SingleOperation {oOpr = Natural Branch {
       oBranchIs = [General NullInstruction, TargetInstruction TBX_RET]}}
  :
  code) constraints =
  let alt = XorExpr (ActiveExpr (oId op)) (ActiveExpr (oId or))
  in (code, constraints ++ [alt])

altRetConstraints (_ : code) constraints = (code, constraints)

altLoadStoreConstraints (
  s1 @ SingleOperation {oOpr = Natural Linear {
       oIs = General NullInstruction : is1}}
  :
  s2 @ SingleOperation {oOpr = Natural Linear {
       oIs = General NullInstruction : is2}}
  :
  ds @ SingleOperation {oOpr = Natural Linear {
       oIs = [General NullInstruction, TargetInstruction dsi]}}
  :
  code) constraints
  | all isSingleLoadStore (map oTargetInstr (is1 ++ is2)) &&
    isDoubleLoadStore dsi =
    let alt =
          XorExpr
          (AndExpr [ActiveExpr (oId s1), ActiveExpr (oId s2)])
          (ActiveExpr (oId ds))
    in (code, constraints ++ [alt])

altLoadStoreConstraints (_ : code) constraints = (code, constraints)

isSingleLoadStore i = i `elem` [TLDRi, T2LDRi12, TSTRi, T2STRi12]
isDoubleLoadStore i = i `elem` [T2LDRDi8, T2STRDi8]
