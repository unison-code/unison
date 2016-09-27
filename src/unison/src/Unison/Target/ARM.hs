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
module Unison.Target.ARM (target) where

import Data.List
import qualified Data.Set as S

import Common.Util

import MachineIR hiding (parse)

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Analysis.TemporaryType

import Unison.Target.ARM.Common
import Unison.Target.ARM.Registers
import Unison.Target.ARM.OperandInfo
import Unison.Target.ARM.Transforms
import Unison.Target.ARM.ARMRegisterDecl
import Unison.Target.ARM.ARMRegisterClassDecl
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
      API.tPreProcess       = const preProcess,
      API.tPostProcess      = const postProcess,
      API.tTransforms       = const transforms,
      API.tCopies           = const copies,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = const operandInfo,
      API.tAlignedPairs     = const SpecsGen.alignedPairs,
      API.tResources        = const resources,
      API.tUsages           = const usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const SpecsGen.readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = const readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy
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

branchInfo (Branch {oBranchIs = is, oBranchUs = [_, _]})
  | targetInst is `elem`  [TBX_RET] =
    BranchInfo Unconditional Nothing

branchInfo (Branch {oBranchIs = is, oBranchUs = _})
  | targetInst is `elem`  [T2LDMIA_RET, TPOP_RET, T2BR_JT, T2TBB_JT, T2TBH_JT] =
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
       then [mkNullInstruction, TargetInstruction TPUSHcs]
       else [],
       [if isExitTemp (fCode f) t
        then [mkNullInstruction, TargetInstruction (popInstruction f t rs)]
        else []])

-- Do not extend non-pre-allocated temporaries that are only "passed through" a
-- block without calls
copies (f, _, _, _, _, _) False t [] d [u]
    | isIn d && isOut u && not (any isCall (bCode $ tempBlock (fCode f) t)) =
    ([], [[]])

copies (f, _, cg, ra, _, _) _ t _rs d us =
  let is = d:us
      w  = widthOfTemp ra cg f t is
  in (
      (defCopies t w d),
      map (useCopies t w) us
      )

popInstruction f t [R4_7]
  | none isTailCall $ bCode $ tempBlock (fCode f) t = TPOPcs_free
popInstruction _ _ _ = TPOPcs

defCopies _ w _ =
  [mkNullInstruction] ++
   map TargetInstruction (moveInstrs w) ++
   map TargetInstruction (storeInstrs w)

useCopies _ w _ =
  [mkNullInstruction] ++
   map TargetInstruction (moveInstrs w) ++
   map TargetInstruction (loadInstrs w)

widthOfTemp = widthOf (target, [])

-- TODO: add STORE_S, LOAD_S, also GPR <-> SPR moves?

-- TMOVr, {TMOVr, VMOVS, VMOVSR, VMOVRS} (MOVE_ALL is instanciated after
-- register allocation since their properties are all the same -- except TMOVr
-- has size 2)
moveInstrs 1 = [MOVE, MOVE_ALL]
-- VMOVD
moveInstrs 2 = [MOVE_D]

-- {T2STRi12, tSTRi}
storeInstrs 1 = [STORE, STORE_T]
-- VSTRD
storeInstrs 2 = [STORE_D]

-- {T2LDRi12, tLDRi}
loadInstrs 1 = [LOAD, LOAD_T]
-- VLDRD
loadInstrs 2 = [LOAD_D]

isReserved r = r `elem` reserved

-- | Transforms copy instructions into natural instructions

fromCopy Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MOVE, MOVE_ALL, MOVE_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs = [s] ++ defaultPred',
            oDs = [d]}
  | i `elem` [STORE, STORE_T, STORE_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs  = [mkOprArmSP, mkBoundMachineFrameObject i d, s] ++
                   defaultPred',
            oDs  = []}
  | i `elem` [LOAD, LOAD_T, LOAD_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i (s, d))],
            oUs  = [mkOprArmSP, mkBoundMachineFrameObject i s] ++
                   defaultPred',
            oDs  = [d]}
  | i `elem` [TPUSHcs] =
    Linear {oIs = [TargetInstruction i],
            oUs = [s],
            oDs = [mkBound mkMachineNullReg]}
  | i `elem` [TPOPcs, TPOPcs_free] =
    Linear {oIs = [TargetInstruction i],
            oUs = [mkBound mkMachineNullReg],
            oDs = [d]}

fromCopy o = error ("unmatched pattern: fromCopy " ++ show o)

mkOprArmSP = Register $ mkTargetRegister SP

mkBoundMachineFrameObject i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size)

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

isSPR r = rTargetReg (regId r) `elem` registers (RegisterClass SPR)
isGPR r = rTargetReg (regId r) `elem` registers (RegisterClass GPR)

-- | Declares target architecture resources

data MipsResource =
  BundleWidth |
  V6_Pipe
  deriving (Eq, Ord, Show, Read)

resources =
    [

     -- Boundle width

     Resource BundleWidth 8,

     -- Resources as defined by ARMScheduleV6

     Resource V6_Pipe 1

    ]

-- | Declares resource usages of each instruction

usages i =
  let it = SpecsGen.itinerary i
  -- TODO: define instruction size as BundleWidth usage
  in mergeUsages (itineraryUsage i it)
     [Usage BundleWidth (SpecsGen.size i) 1]

itineraryUsage _ it
  | it `elem` [NoItinerary] = []
  | it `elem` [IIC_Br, IIC_iALUi, IIC_iBITi, IIC_iCMPi, IIC_iEXTr,
               IIC_iLoad_bh_r, IIC_iLoad_r, IIC_iMOVi, IIC_iMVNi, IIC_fpCVTID,
               IIC_iALUr, IIC_iCMOVr, IIC_iCMPr, IIC_iLoad_bh_ru, IIC_iMVNr,
               IIC_iStore_r, IIC_iTSTi, IIC_iTSTr, IIC_iUNAsi, IIC_fpLoad32,
               IIC_fpLoad64, IIC_iBITr, IIC_iCMOVi, IIC_iCMOVsr, IIC_iEXTAr,
               IIC_iLoad_iu, IIC_iMAC16, IIC_iStore_bh_iu, IIC_iStore_bh_r,
               IIC_iStore_bh_ru, IIC_iStore_d_r, IIC_iStore_iu, IIC_fpALU32,
               IIC_fpCMP64, IIC_fpCVTDS, IIC_fpMOVID, IIC_iLoad_ru, IIC_iMUL16,
               IIC_fpALU64, IIC_fpSTAT, IIC_fpStore32, IIC_fpStore64,
               IIC_iStore_ru, IIC_fpCVTSD, IIC_fpMUL32, IIC_fpCVTIS,
               IIC_fpCVTSI, IIC_fpMOVDI, IIC_iMOVr, IIC_fpUNA64, IIC_fpMOVIS,
               IIC_iALUsi, IIC_iALUsir, IIC_iBITsi, IIC_iLoad_bh_i, IIC_iLoad_i,
               IIC_iMOVsi, IIC_iStore_i, IIC_iCMOVsi, IIC_iCMPsi,
               IIC_iLoad_bh_iu, IIC_iStore_bh_i, IIC_iALUx, IIC_iLoad_d_i,
               IIC_iUNAr, IIC_fpMOVSI, IIC_iTSTsi, IIC_fpCMP32, IIC_fpCVTDI,
               IIC_fpUNA32] =
      [Usage V6_Pipe 1 1]
  | it `elem` [IIC_iALUsr, IIC_iMOVsr, IIC_iBITsr, IIC_iCMPsr, IIC_iMOVix2,
               IIC_iMUL32, IIC_iLoad_si, IIC_iCMOVix2, IIC_iLoad_bh_si,
               IIC_iStore_si, IIC_fpMUL64, IIC_iMAC32, IIC_iStore_bh_si,
               IIC_iTSTsr] =
      [Usage V6_Pipe 1 2]
  | it `elem` [IIC_iStore_mu, IIC_iStore_m, IIC_iLoad_mu, IIC_iLoad_m,
               IIC_fpStore_mu, IIC_fpLoad_mu, IIC_iPop, IIC_fpLoad_m,
               IIC_fpStore_m] =
      [Usage V6_Pipe 1 3]
  | it `elem` [IIC_iLoad_mBr, IIC_iPop_Br] =
      [Usage V6_Pipe 1 4]
  | it `elem` [IIC_fpDIV32] =
      [Usage V6_Pipe 1 15]
  | it `elem` [IIC_fpSQRT64, IIC_fpDIV64] =
      [Usage V6_Pipe 1 29]

itineraryUsage _ it = error ("unmatched: itineraryUsage " ++ show it)

-- | No-operation instruction

nop = Linear [TargetInstruction NOP] [] []

-- | Implementation of frame setup and destroy operations. All functions
-- observed so far have a reserved call frame (hasReservedCallFrame(MF)), which
-- means frame setup and destroy operations are just removed (see
-- ARMFrameLowering.cpp ("eliminateCallFramePseudoInstr")).

implementFrame = const []

-- | Adds function prologue, see corresponding logic in ARMFrameLowering.cpp
-- ("emitPrologue")

{-

Observations:

ArgRegsSaveSize is always 0

a function has a stack frame (AFI->hasStackFrame()) iff
 - BigStack (always false), or
 - !CanEliminateFrame (sometimes true, sometimes false), or
 - RegInfo->cannotEliminateFrame(MF) (sometimes true, sometimes false)

CanEliminateFrame <-> no callee-saved register is spilled

RegInfo->cannotEliminateFrame(MF) <->
  MFI->adjustsStack() (sometimes true, sometimes false) ||
  MFI->hasVarSizedObjects() (always false) ||
  MFI->isFrameAddressTaken() (always false) ||
  needsStackRealignment(MF) (alwaysFalse)

MFI->adjustsStack() <-> exists a FrameSetupOpcode or a FrameDestroyOpcode
(calculateCallsInformation in PrologEpilogInserter.cpp)

to summarize:

hasStackFrame() <->
a callee-saved register is spilled ||
exists a FrameSetupOpcode or a FrameDestroyOpcode

prologue:

if !hasStackFrame() -> no prologue
otherwise: ?

-}

addPrologue _ code = code

-- | Adds function epilogue (TODO: investigate crashes for ARM, see "emitEpilogue" in ARMFrameLowering.cpp)

addEpilogue _ code = code

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess = [cleanConstPoolBlocks, hideCPSRRegister,
              addFrameIndex, processTailCalls, dropUnneededInstructions,
              reorderImplicitOperands]

cleanConstPoolBlocks mf @ MachineFunction {mfBlocks = mbs} =
  mf {mfBlocks = filter (not . isConstPoolBlock) mbs}

isConstPoolBlock MachineBlock {mbInstructions = mis} =
  (not $ null mis) &&
  all (\mi ->
         (isMachineTarget mi && mopcTarget (msOpcode mi) == CONSTPOOL_ENTRY) ||
         (isMachineVirtual mi && mopcVirtual (msOpcode mi) == EXIT)) mis

-- This is done to prevent 'extractCallRegs' adding the 'cpsr' register as
-- argument to function calls

hideCPSRRegister = mapToMachineInstruction hideCPSRRegisterInInstr

hideCPSRRegisterInInstr mi @ MachineSingle {msOperands = mos} =
  let mos' = mapIf isMachineReg hideCPSR mos
  in mi {msOperands = mos'}

hideCPSR mr @ MachineReg {mrName = CPSR} = mr {mrName = PRED}
hideCPSR mr = mr

liftToTOpc f = mkMachineTargetOpc . f . mopcTarget

addFrameIndex = mapToTargetMachineInstruction addFrameIndexInstr

addFrameIndexInstr mi @ MachineSingle {msOpcode = opcode,
                                       msOperands = operands}
  | any isMachineConstantPoolIndex operands &&
    any isTemporaryInfo (fst $ operandInfo $ mopcTarget opcode) =
    mi {msOpcode = liftToTOpc (\i -> read (show i ++ "_cpi")) opcode}

addFrameIndexInstr mi @ MachineSingle {msOpcode = opcode,
                                       msOperands = operands}
  | any isMachineFrameIndex operands &&
    any isTemporaryInfo (fst $ operandInfo $ mopcTarget opcode) =
      mi {msOpcode = liftToTOpc (\i -> read (show i ++ "_fi")) opcode}
  | otherwise = mi

processTailCalls = mapToTargetMachineInstruction processTailCallsInInstr

processTailCallsInInstr mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                            msOperands = s:mos}
  | i == TCRETURNdi =
    let mos' = [s] ++ defaultPred ++ mos
    in mi {msOpcode = mkMachineTargetOpc TTAILJMPdND, msOperands = mos'}
  | i == TCRETURNri = mi {msOpcode = mkMachineTargetOpc TTAILJMPr}

processTailCallsInInstr mi = mi

dropUnneededInstructions =
  filterMachineInstructions
  (\mi -> not (isMachineTarget mi &&
               mopcTarget (msOpcode mi) == CFI_INSTRUCTION))

reorderImplicitOperands = mapToMachineInstruction reorderImplicitOperandsInInstr

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [o1, o2, p1, p2,
                                    cc @ MachineReg {mrName = PRED}]}
  | i `elem` [T2CMPri, T2TSTri, T2CMNri, T2CMPrr, T2TSTrr, T2SUBrr, T2TEQrr] =
    let mos' = [cc, o1, o2, p1, p2]
    in mi {msOperands = mos'}

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [o1, o2, o3, p1, p2,
                                    cc @ MachineReg {mrName = PRED}]}
  | i `elem` [T2CMPrs, T2TSTrs] =
    let mos' = [cc, o1, o2, o3, p1, p2]
    in mi {msOperands = mos'}

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [o1, o2, o3, p1, p2,
                                    cc @ MachineReg {mrName = PRED}]}
  | i `elem` [T2SUBrr, T2SUBri, T2ORRrr, T2ANDri, T2ADDri] =
    let mos' = [o1, cc, o2, o3, p1, p2]
    in mi {msOpcode = mkMachineTargetOpc (toExplicitCpsrDef i),
           msOperands = mos'}

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [p1, p2,
                                    cc @ MachineReg {mrName = PRED},
                                    fp]}
  | i `elem` [FMSTAT] =
    let mos' = [cc, p1, p2, fp]
    in mi {msOperands = mos'}

reorderImplicitOperandsInInstr mi = mi

-- | Target dependent post-processing functions

postProcess = [expandPseudos, removeAllNops, removeFrameIndex,
               postReorderImplicitOperands, exposeCPSRRegister]

expandPseudos = mapToMachineBlock (expandBlockPseudos expandPseudo)

expandBlockPseudos f mi @ MachineBlock {mbInstructions = mis} =
    let mis'   = map miToList mis
        mis''  = expand f mis'
        mis''' = map listToMi mis''
    in mi {mbInstructions = mis'''}

expandPseudo mi @ MachineSingle {
  msOpcode   = MachineTargetOpc T2MOVi32imm,
  msOperands = [dst, ga @ MachineGlobalAddress {}]} =
  let mi1 = mi {msOpcode   = mkMachineTargetOpc T2MOVi16,
                msOperands = [dst, ga] ++ defaultPred}
      mi2 = mi {msOpcode   = mkMachineTargetOpc T2MOVTi16,
                msOperands = [dst, dst, ga] ++ defaultPred}
  in [[mi1], [mi2]]

expandPseudo mi = [[mi]]

removeAllNops =
  filterMachineInstructions
  (\mi -> not (isMachineTarget mi && mopcTarget (msOpcode mi) == NOP))

defaultPred = [mkMachineImm 14, mkMachineNullReg]

defaultPred' = map mkBound defaultPred

postReorderImplicitOperands =
  mapToMachineInstruction postReorderImplicitOperandsInInstr

postReorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [cc @ MachineReg {mrName = CPSR},
                                    o1, o2, p1, p2]}
  | i `elem` [T2TSTri_cpsr, T2CMNri_cpsr, T2CMPrr_cpsr, T2TSTrr_cpsr,
              T2SUBrr_cpsr, TCMPi8_cpsr] =
    let mos' = [o1, o2, p1, p2, cc]
    in mi {msOpcode = mkMachineTargetOpc $ fromExplicitCpsrDef i,
           msOperands = mos'}

postReorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | i `elem` [T2CMPri_cpsr] =
    mi {msOpcode = MachineTargetOpc $ fromExplicitCpsrDef i}

postReorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [d, u1, p1, p2, _]}
  | i == TMOVi8s =
    let mos' = [d, mkMachineReg CPSR, u1, p1, p2]
    in mi {msOpcode = mkMachineTargetOpc TMOVi8, msOperands = mos'}

postReorderImplicitOperandsInInstr mi = mi

exposeCPSRRegister = mapToMachineInstruction exposeCPSRRegisterInInstr

exposeCPSRRegisterInInstr mi @ MachineSingle {msOperands = mos} =
  let mos' = mapIf isMachineReg exposeCPSR mos
  in mi {msOperands = mos'}

exposeCPSR mr @ MachineReg {mrName = PRED} = mr {mrName = CPSR}
exposeCPSR mr = mr

removeFrameIndex = mapToMachineInstruction removeFrameIndexInstr

removeFrameIndexInstr mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | "_fi" `isSuffixOf`  (show i) =
    mi {msOpcode = mkMachineTargetOpc $ read $ dropSuffix "_fi" (show i)}
  | "_cpi" `isSuffixOf` (show i) =
    mi {msOpcode = mkMachineTargetOpc $ read $ dropSuffix "_cpi" (show i)}
  | otherwise = mi



-- | Gives a list of function transformers
transforms = [peephole extractReturnRegs, peephole foldResRegAssignment,
              peephole cleanResRegCopies, mapToOperation addThumbAlternatives,
              peephole handlePromotedOperands]

-- | Latency of read-write dependencies

readWriteLatency _ (_, Read) (_, Write) = 0
readWriteLatency _ ((_, VirtualType (DelimiterType InType)), _) (_, _) = 1
readWriteLatency _ ((_, VirtualType FunType), _) (_, _) = 1
readWriteLatency _ ((_, VirtualType _), _) (_, _) = 0
readWriteLatency _ ((TargetInstruction p, _), _) (_, _) =
    maybeMax 0 $ map occupation (usages p)


-- | Alternative temporaries of each operand

-- All temps that hold the same value

alternativeTemps _ _ _ ts = map fst ts

-- | Copy expansion

expandCopy _ _ o = [o]
