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
import MachineIR.Transformations.AddImplicitRegs

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Analysis.TemporaryType
import Unison.Transformations.FoldReservedRegisters

import Unison.Target.ARM.Common
import Unison.Target.ARM.Registers
import Unison.Target.ARM.OperandInfo
import Unison.Target.ARM.Transforms
import Unison.Target.ARM.Usages
import Unison.Target.ARM.ARMRegisterDecl
import Unison.Target.ARM.ARMRegisterClassDecl
import Unison.Target.ARM.ARMResourceDecl
import Unison.Target.ARM.SpecsGen.ARMInstructionDecl
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
      API.tPackedPairs      = const (const (const [])),
      API.tRelatedPairs     = const (const []),
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
       then [mkNullInstruction, TargetInstruction (pushInstruction rs)]
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
  let is     = d:us
      w      = widthOfTemp ra cg f t is
      Just g = fGoal f
  in (
      (defCopies g t w d),
      map (useCopies g t w) us
      )

pushInstruction [r]
  | r `elem` registers (RegisterClass CS) = TPUSHcs
  | r `elem` registers (RegisterClass DPR) = STORE_D

popInstruction f t [r]
  | r == R4_7 && (none isTailCall $ bCode $ tempBlock (fCode f) t) = TPOPcs_free
  | r `elem` registers (RegisterClass CS) = TPOPcs
  | r `elem` registers (RegisterClass DPR) = LOAD_D

defCopies g _ w _ =
  [mkNullInstruction] ++
   map TargetInstruction (moveInstrs g w) ++
   map TargetInstruction (storeInstrs g w)

useCopies g _ w _ =
  [mkNullInstruction] ++
   map TargetInstruction (moveInstrs g w) ++
   map TargetInstruction (loadInstrs g w)

widthOfTemp = widthOf (target, [])

-- TODO: add STORE_S, LOAD_S, also GPR <-> SPR moves?

-- TMOVr, {TMOVr, VMOVS, VMOVSR, VMOVRS} (MOVE_ALL is instanciated after
-- register allocation since their properties are all the same -- except TMOVr
-- has size 2)
moveInstrs g 1
  | g == Speed = [MOVE_ALL]
  | g == Size = [MOVE, MOVE_ALL]
-- VMOVD
moveInstrs _ 2 = [MOVE_D]

-- {T2STRi12, tSTRi}
storeInstrs g 1
  | g == Speed = [STORE]
  | g == Size = [STORE, STORE_T]
-- VSTRD
storeInstrs _ 2 = [STORE_D]

-- {T2LDRi12, tLDRi}
loadInstrs g 1
  | g == Speed = [LOAD]
  | g == Size = [LOAD, LOAD_T]
-- VLDRD
loadInstrs _ 2 = [LOAD_D]

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

resources =
    [

     -- Boundle width (times 16 bits)

     Resource BundleWidth 4,

     -- Resources as defined by ARMScheduleV6

     Resource V6_Pipe 1

    ]

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

preProcess = [cleanConstPoolBlocks, promoteImplicitOperands, hideCPSRRegister,
              addFrameIndex, processTailCalls]

cleanConstPoolBlocks mf @ MachineFunction {mfBlocks = mbs} =
  mf {mfBlocks = filter (not . isConstPoolBlock) mbs}

isConstPoolBlock MachineBlock {mbInstructions = mis} =
  (not $ null mis) &&
  all (\mi ->
         (isMachineTarget mi && mopcTarget (msOpcode mi) == CONSTPOOL_ENTRY) ||
         (isMachineVirtual mi && mopcVirtual (msOpcode mi) == EXIT)) mis

promoteImplicitOperands = mapToMachineInstruction promoteImplicitOperandsInInstr

promoteImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [o1, o2, o3, p1, p2,
                                    cc @ MachineReg {mrName = CPSR}]}
  | i `elem` [T2SUBrr, T2SUBri, T2ORRrr, T2ANDri, T2ADDri] =
    let mos' = [o1, cc, o2, o3, p1, p2]
    in mi {msOpcode = mkMachineTargetOpc (toExplicitCpsrDef i),
           msOperands = mos'}

promoteImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = mos} =
    let fu   = length $ snd $ operandInfo i
        mos' = if writesSideEffect i CPSR
               then insertAt (mkMachineReg CPSR) (fu - 1) mos
               else mos
    in mi {msOperands = mos'}

promoteImplicitOperandsInInstr mi = mi

writesSideEffect i eff =
    (OtherSideEffect eff) `elem` (snd $ SpecsGen.readWriteInfo i)

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

-- | Target dependent post-processing functions

postProcess = [expandPseudos, removeAllNops, removeFrameIndex,
               reorderImplicitOperands, exposeCPSRRegister,
               flip addImplicitRegs (target, []), demoteImplicitOperands]

expandPseudos = mapToMachineBlock (expandBlockPseudos expandPseudo)

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

reorderImplicitOperands = mapToMachineInstruction reorderImplicitOperandsInInstr

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                      msOperands = [cc @ MachineReg {mrName = CPSR},
                                    o1, o2, p1, p2]}
  | i `elem` [T2TSTri_cpsr, T2CMNri_cpsr, T2CMPrr_cpsr, T2TSTrr_cpsr,
              T2SUBrr_cpsr, TCMPi8_cpsr] =
    let mos' = [o1, o2, p1, p2, cc]
    in mi {msOpcode = mkMachineTargetOpc $ fromExplicitCpsrDef i,
           msOperands = mos'}

reorderImplicitOperandsInInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | i `elem` [T2CMPri_cpsr] =
    mi {msOpcode = MachineTargetOpc $ fromExplicitCpsrDef i}

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

removeFrameIndexInstr mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | "_fi" `isSuffixOf`  (show i) =
    mi {msOpcode = mkMachineTargetOpc $ read $ dropSuffix "_fi" (show i)}
  | "_cpi" `isSuffixOf` (show i) =
    mi {msOpcode = mkMachineTargetOpc $ read $ dropSuffix "_cpi" (show i)}
  | otherwise = mi

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

-- | Gives a list of function transformers
transforms ImportPreLift = [peephole extractReturnRegs,
                            (\f -> foldReservedRegisters f (target, [])),
                            mapToOperationWithGoal addThumbAlternatives]
transforms ImportPostLift = [peephole handlePromotedOperands]
transforms _ = []

mapToOperationWithGoal t f @ Function {fCode = code, fGoal = Just goal} =
  f {fCode = map (mapToOperationInBlock (t goal)) code}

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
