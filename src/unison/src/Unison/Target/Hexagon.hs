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
module Unison.Target.Hexagon (target) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List.Split
import Control.Arrow

import Common.Util

import MachineIR hiding (parse)
import MachineIR.Transformations.AddImplicitRegs

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Analysis.TemporaryType
import Unison.Target.Query
import Unison.Target.Hexagon.Registers
import Unison.Target.Hexagon.Transforms
import Unison.Target.Hexagon.Usages
import Unison.Target.Hexagon.Common
import Unison.Target.Hexagon.HexagonRegisterDecl
import Unison.Target.Hexagon.HexagonRegisterClassDecl
import Unison.Target.Hexagon.HexagonResourceDecl
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl
import qualified Unison.Target.Hexagon.SpecsGen as SpecsGen

target :: API.TargetDescription
          HexagonInstruction
          HexagonRegister
          HexagonRegisterClass
          HexagonResource
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
      API.tPostProcess      = const postProcess,
      API.tTransforms       = transforms,
      API.tCopies           = const copies,
      API.tRematInstrs      = const rematInstrs,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = operandInfo,
      API.tAlignedPairs     = const SpecsGen.alignedPairs,
      API.tPackedPairs      = const (const (const [])),
      API.tRelatedPairs     = const (const []),
      API.tResources        = resources,
      API.tUsages           = usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = const readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy,
      API.tConstraints      = constraints
    }

-- | Gives the type of natural operation according to the instruction

instructionType i
    | i `elem` [MVW, MVD, STW, STD, STD_cs, LDW, LDD, MVPR, MVRP, STW_nv] =
        CopyInstructionType
    | i `elem` [TCRETURNi, TCRETURNi_ce] = TailCallInstructionType
    | otherwise = SpecsGen.instructionType i

-- | Gives the target of a jump instruction and the type of jump

branchInfo (Branch {oBranchIs = ops, oBranchUs = [_, BlockRef l]})
  | targetInst ops `elem` [J2_jumpt, J2_jumpt_nv, J2_jumpt_ce, J2_jumpf, J2_jumpf_nv,
                           J2_jumpf_ce, J2_jumptnewpt, J2_jumptnew,
                           J2_jumpfnewpt, J2_jumpfnew] =
    BranchInfo Conditional (Just l)

-- | New-value compare jumps
branchInfo (Branch {oBranchUs = [_, BlockRef l]}) =
  BranchInfo Conditional (Just l)
branchInfo (Branch {oBranchUs = [_, _, BlockRef l]}) =
  BranchInfo Conditional (Just l)

branchInfo (Branch {oBranchIs = ops, oBranchUs = [BlockRef l]})
  | targetInst ops `elem` [ENDLOOP0, ENDLOOP1] = BranchInfo Conditional (Just l)

branchInfo (Branch {oBranchIs = ops, oBranchUs = [BlockRef l]})
  | targetInst ops `elem` [J2_jump, J2_jump_ce] =
    BranchInfo Unconditional (Just l)

branchInfo (Branch {oBranchIs = ops})
  | targetInst ops `elem`  [JMPret, JMPrett, JMPretf, JMPrettnew, JMPretfnew,
                            L4_return, L4_return_t, L4_return_f,
                            L4_return_tnew_pt, L4_return_fnew_pt, J2_jumpr,
                            Jr_merge] =
    BranchInfo Unconditional Nothing

branchInfo (Branch {oBranchIs = ops})
  | targetInst ops `elem`  [J2_jumprt, J2_jumprf] =
    BranchInfo Conditional Nothing

-- | Tail calls at pre-emit stage

branchInfo (Branch {oBranchIs = ops,
                    oBranchUs = [_, Bound (MachineGlobalAddress {})]})
  | targetInst ops `elem` [J2_jumpt, J2_jumpf] =
    BranchInfo Conditional Nothing

branchInfo (Branch {oBranchIs = ops,
                    oBranchUs = [Bound (MachineGlobalAddress {})]})
  | targetInst ops `elem` [J2_jump] =
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
copies (f, cst, cg, ra, _, _) False t rs d [u]
  | not (null rs) && S.member t cst =
    let is = [d, u]
        w  = widthOfTemp ra cg f t is
    in
      (
       if isEntryTemp (fCode f) t
       then [mkNullInstruction, TargetInstruction (calleeSavedStoreOp w)]
       else [],
       [if isExitTemp (fCode f) t
        then [mkNullInstruction, TargetInstruction (loadOp w)]
        else []]
      )

-- Do not extend non-pre-allocated temporaries that are only "passed through" a
-- block without calls
copies (f, _, _, _, _, _) False t [] d [u]
    | isIn d && isOut u && not (any isCall (bCode $ tempBlock (fCode f) t)) =
    ([], [[]])

-- Do not extend predicate temporaries generated by compares and the like and
-- directly consumed by jumps
copies _ False t [] d [u]
  | isNatural d &&
    (isBranch u ||
    (isNatural u &&
     targetInst (oInstructions u) `elem` [C2_not, C2_xor, C2_and])) &&
    fromJust (classOfTemp t d) `elem` (map RegisterClass [PredRegs, F32]) =
      ([], [[]])

-- Do not extend rematerializable instructions used only once, locally
-- FIXME: review whether this is always safe
copies (Function {fCode = code}, _, _, _, _, _) False t _ d [u]
  | isNatural d && (isNatural u || isFun u) &&
    (isRematerializable (targetInst (oInstructions d))) &&
    not (mayCrossMemDep SpecsGen.readWriteInfo d u code) &&
    compatibleClassesForTemp t [d, u] = ([], [[]])

copies (f, _, cg, ra, _, _) _ t _rs d us =
  let is = d:us
      w  = widthOfTemp ra cg f t is
  in (
      (defCopies t w d),
      map (useCopies t w) us
      )

defCopies t w d =
  case classOfTemp t d of
    Just (RegisterClass PredRegs) -> [mkNullInstruction,
                                      TargetInstruction predToRegOp]
    _                          -> [mkNullInstruction,
                                   TargetInstruction (moveOp w),
                                   TargetInstruction (storeOp w)] ++
                                  (fmap TargetInstruction (newValueStoreOp d w))

useCopies t w u =
  case classOfTemp t u of
    Just (RegisterClass PredRegs) -> [mkNullInstruction,
                                      TargetInstruction regToPredOp]
    _                          -> [mkNullInstruction,
                                   TargetInstruction (moveOp w),
                                   TargetInstruction (loadOp w)]

classOfTemp = classOf (target, [])
widthOfTemp = widthOf (target, [])

compatibleClassesForTemp t os =
  let regs = [S.fromList $ registers $ fromJust (classOfTemp t o) | o <- os]
  in not $ S.null $ foldl S.intersection (head regs) regs

moveOp 1 = MVW
moveOp 2 = MVD

storeOp 1 = STW
storeOp 2 = STD

calleeSavedStoreOp 1 = STW
calleeSavedStoreOp 2 = STD_cs

loadOp 1 = LDW
loadOp 2 = LDD

predToRegOp = MVPR
regToPredOp = MVRP

newValueStoreOp d w
  | isDelimiter d       = []
  | isCall d            = []
  | isLow d || isHigh d = []
  | w /= 1              = []
  | otherwise           = [STW_nv]

isReserved r = r `elem` reserved

rematInstrs i
  | isRematerializable i =
      Just (sourceInstr i, dematInstr i, rematInstr i)
  | otherwise = error ("unmatched: rematInstrs " ++ show i)

-- | Transforms copy instructions into natural instructions

-- handle regular copies
fromCopy _ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MVW, MVD, MVPR, MVRP] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [s],
            oDs = [d]}
  | i `elem` [STW, STD, STD_cs, STW_nv] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [mkOprHexagonSP, mkBoundMachineFrameObject i d, s],
            oDs = []}
  | i `elem` [LDW, LDD] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [mkOprHexagonSP, mkBoundMachineFrameObject i s],
            oDs = [d]}

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

mkOprHexagonSP = Register $ mkTargetRegister hexagonSP

mkBoundMachineFrameObject i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r * 4) (Just size) size False)

stackSize op
  | op `elem` [STW, STW_nv, LDW] = 4
  | op `elem` [STD, STD_cs, LDD] = 8

fromCopyInstr i
  | isJust (SpecsGen.parent i) = fromJust (SpecsGen.parent i)
fromCopyInstr STW = S2_storeri_io
fromCopyInstr i
  | i `elem` [STD, STD_cs] = S2_storerd_io
fromCopyInstr STW_nv = S2_storerinew_io
fromCopyInstr LDW = L2_loadri_io
fromCopyInstr LDD = L2_loadrd_io


-- | Gives information about the operands of each instruction

-- TODO: this is terrible! we need to respecify the operand info of all
-- instructions with use latency -1... can we do this better? (e.g. via custom
-- Haskell functions specified in hexagon-manual.yaml).
operandInfo to i
  | i `elem`
      [J2_jumpfnew, J2_jumpfnewpt, J2_jumptnew, J2_jumptnewpt]
    = ([TemporaryInfo (RegisterClass F32) (-1) True, BlockRefInfo], [])
  | i `elem` [A2_tfrt, A2_tfrf] =
    ([TemporaryInfo (RegisterClass PredRegs) (-1) True,
      TemporaryInfo (RegisterClass IntRegs) 0 False],
     [TemporaryInfo (RegisterClass IntRegs) 1 False])
  -- New-value stores (the last use of class IntRegs has latency -1)
  | baseInstr i `elem` [S2_storerinew_io, S2_storerbnew_io, S2_storerhnew_io] =
    ([TemporaryInfo (RegisterClass IntRegs) 0 False, BoundInfo,
      TemporaryInfo (RegisterClass IntRegs) (-1) True],
     [])
  | baseInstr i `elem` [S2_storerbnew_pi, S2_storerhnew_pi, S2_storerinew_pi] =
    ([TemporaryInfo (RegisterClass IntRegs) 0 False, BoundInfo,
      TemporaryInfo (RegisterClass IntRegs) (-1) True],
     [TemporaryInfo (RegisterClass IntRegs) 1 False])
  | baseInstr i `elem` [S2_storerinewabs] =
    ([BoundInfo, TemporaryInfo (RegisterClass IntRegs) (-1) True], [])
  | baseInstr i `elem` [S2_storerinew_io_fi] =
    ([BoundInfo, BoundInfo, TemporaryInfo (RegisterClass IntRegs) (-1) True],
     [])
  | i `elem` [S4_storerhnew_rr, S4_storerinew_rr] =
    ([TemporaryInfo (RegisterClass IntRegs) 0 False,
      TemporaryInfo (RegisterClass IntRegs) 0 False, BoundInfo,
      TemporaryInfo (RegisterClass IntRegs) (-1) True],
     [])
    -- Mark uses in merge return instructions as bypassing
  | i `elem` [Jr_merge, Ret_dealloc_merge] =
      first (map markAsBypass) $ SpecsGen.operandInfo i
  | isComboNVCJ i && usePredicateRegsForNvcj to =
      mapTuple (map f32ToPredRegs) (SpecsGen.operandInfo i)
  | otherwise = SpecsGen.operandInfo i

isComboNVCJ i = "combo" `isInfixOf` (show i)

markAsBypass ti @ TemporaryInfo {} = ti {oiBypassing = True}
markAsBypass pi = pi

f32ToPredRegs ti @ TemporaryInfo {oiRegClass = RegisterClass F32} =
  ti {oiRegClass = RegisterClass PredRegs}
f32ToPredRegs oi = oi

baseInstr i
  | isConstantExtended i = nonConstantExtendedInstr i
  | otherwise = i

resources to
  | singleIssue to = [Resource BundleWidth 1, Resource BlockEnd 1]
  | otherwise =
    [

     -- Boundle width

     Resource BundleWidth 4,

     -- Issue slots hierarchy (see Fig. 1-3 in "Hexagon V2 Programmer's
     -- Reference Manual" and HexagonScheduleV4.td in LLVM back-end)

     Resource Slot0123 4, -- Used by ALU32
     Resource Slot01 2,   -- Used by LD and ST
     Resource Slot0 1,    -- Used by MEMOP, NV (NVST), SYSTEM
     Resource Slot23 2,   -- Used by XTYPE and J
     Resource Slot2 1,    -- Used by JR
     Resource Slot3 1,    -- Used by CR

     -- Artificial resource to represent the fact that there can only be up
     -- to two regular stores or a single new-value store per bundle (see
     -- Section 5.5 in Hexagon programmer's reference). Also used to avoid
     -- stores in parallel with L4_return (seems to be disallowed by LLVM)
     Resource Store 2,    -- Used by ST and NVST

     -- Artificial resource to forbid conditional instructions and new-value
     -- stores in the same bundle where the former feeds the latter, which is
     -- forbidded (TODO: this prevents some potentially legal bundles if there
     -- is no data dependency, relate such instructions more directly)
     Resource ConNewValue 1,

     -- Artificial resource to disallow jump merges and ENDLOOP instructions to
     -- be scheduled together with (out)-delimiters
     Resource BlockEnd 1,

     -- Artificial resource for weighted spill code minimization
     Resource SpillCost 2

    ]

-- | No-operation instruction

nop = Linear [TargetInstruction A2_nop] [] []

readWriteInfo i
  | i `elem` [JMPret] =
      addRegRead R31 $ SpecsGen.readWriteInfo i
  | i `elem` [JMPret_dealloc_linear, L4_return_linear, JMPret_linear,
              Ret_dealloc_merge] =
      second (++ [ControlSideEffect]) $ SpecsGen.readWriteInfo i
  | otherwise = SpecsGen.readWriteInfo i

addRegRead r = first (++ [OtherSideEffect r])

-- | Implementation of frame setup and destroy operations, see corresponding
-- logic in HexagonFrameLowering.cpp ("eliminateCallFramePseudoInstr")

implementFrame = const []

-- | Adds function prologue, see HexagonFrameLowering.cpp
addPrologue (_, oid, _) (e:code) =
  let af = mkAct $ mkOpt oid S2_allocframe [Bound mkMachineFrameSize] []
  in [e, af] ++ code

-- | Adds function epilogue, see HexagonFrameLowering.cpp
addEpilogue (tid, oid, pid) code =
  case split (keepDelimsL $ whenElt isBranch) code of
   [f,
    SingleOperation {oOpr = Natural Branch {
                        oBranchIs = [TargetInstruction JMPret],
                        oBranchUs = [MOperand {altTemps = [t]}]}}
    :
    e] ->
     let mkOper id ts = mkMOperand (pid + id) ts Nothing
         [t1, t2, t3, t4, t5] = map mkTemp [tid .. tid + 4]
         dfl  = mkOpt oid L2_deallocframe_linear [] [mkOper 0 [t1]]
         jrdl = mkOpt (oid + 1) JMPret_dealloc_linear
                [mkOper 1 [t], mkOper 2 [t1]] [mkOper 3 [t2]]
         rl   = mkOpt (oid + 2) L4_return_linear [] [mkOper 4 [t3]]
         rdm  = mkAct $ mkOpt (oid + 3) Ret_dealloc_merge [mkOper 5 [t2, t3]]
                [mkOper 6 [t4]]
         jrl  = mkOpt (oid + 4) JMPret_linear [mkOper 7 [t]] [mkOper 8 [t5]]
         jrm  = mkBranch (oid + 5) [TargetInstruction Jr_merge]
                [mkOper 9 [t4, t5]]
     in f ++ [dfl, jrdl, rl, rdm, jrl, jrm] ++ e
   [_] ->
     case split (keepDelimsL $ whenElt isTailCall) code of
     [f, e] ->
       let df = mkAct $ mkOpt oid L2_deallocframe [] []
       in f ++ [df] ++ e
     os -> error ("unhandled epilogue: " ++ show os)

mkOpt oid inst us ds =
  makeOptional $ mkLinear oid [TargetInstruction inst] us ds

mkAct = addActivators (map TargetInstruction stackAccessors)

addActivators = mapToActivators . (++)

-- We need a stack frame iff:
stackAccessors =
  -- there are function calls,
  [J2_call, J2_callr] ++
  -- there are instructions referring to frame objects, or
  fiInstrs ++
  -- there are spills.
  spillInstrs

fiInstrs = filter (\i -> "_fi" `isSuffixOf` (show i)) SpecsGen.allInstructions

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess to = [mapToTargetMachineInstruction preprocessJRInstrs,
                 foldSPCopies, addFrameIndex, constantExtend,
                 if singleIssue to then serialize else id]

-- This is so that we take the additional call cost cost of tail-call jumps
-- in LLVM solutions when we run 'uni analyze'
preprocessJRInstrs mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                       msOperands = MachineGlobalAddress {} : _}
  | i `elem` [J2_jump] = mi {msOpcode = mkMachineTargetOpc TCRETURNi}
preprocessJRInstrs mi = mi

foldSPCopies = mapToMachineBlock foldSPCopiesInBlock

foldSPCopiesInBlock mb @ MachineBlock {mbInstructions = mis} =
  let ts    = M.fromList [(t, mkMachineRegSP) | t <- catMaybes $ map spTemp mis]
      mis'  = filter (isNothing . spTemp) mis
      mis'' = map (mapToMachineOperand (applyMap ts)) mis'
  in mb {mbInstructions = mis''}

spTemp ms @ MachineSingle {msOperands = [d, mo]}
    | isMachineCopy ms && isHexagonSP mo = Just d
spTemp _ = Nothing

addFrameIndex = mapToTargetMachineInstruction addFrameIndexInstr

addFrameIndexInstr mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                       msOperands = operands}
  | isMachineVirtual mi = mi
  | any isMachineFrameIndex operands &&
    any isTemporaryInfo (fst $ operandInfo [] i) =
      mi {msOpcode = mkMachineTargetOpc $ read (show i ++ "_fi")}
  | otherwise = mi

isHexagonSP MachineReg {mrName = sp} | sp == hexagonSP = True
isHexagonSP _ = False

constantExtend = mapToTargetMachineInstruction constantExtendInstr

constantExtendInstr mi @ MachineSingle {msOpcode = opcode,
                                        msProperties = ps}
  | any isConstExtendedProperty ps =
    mi {msOpcode     = liftToTOpc constantExtendedInstr opcode,
        msProperties = filter (not . isConstExtendedProperty) ps}
constantExtendInstr mi = mi

liftToTOpc f = mkMachineTargetOpc . f . mopcTarget

isConstExtendedProperty =
  isMachineInstructionPropertyCustomOf "constant-extended"

serialize = mapToMachineBlock serializeBlock

serializeBlock mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = concatMap machineInstructionToList mis}

machineInstructionToList ms @ MachineSingle {} = [ms]
machineInstructionToList MachineBundle {mbInstrs = mis} = mis

-- | Target dependent post-processing functions

postProcess = [lintStackAlignment,
               constantDeExtend, removeFrameIndex,
               normalizeNewValueCmpJump, normalizeNVJumps, normalizeJRInstrs,
               expandCondTransfers, addJumpHints,
               flip addImplicitRegs (target, [])]

lintStackAlignment = mapToTargetMachineInstruction lintStackAlignmentInInstr

lintStackAlignmentInInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc mopc, msOperands = mos}
  | isMemAccessWithOff mopc =
      let imm = miValue $ fromJust $ find isMachineImm mos
          ali = memAccessAlignment mopc
      in if (imm `rem` ali) == 0 then mi
         else error ("missaligned memory access: " ++ show mi)
lintStackAlignmentInInstr mi = mi

constantDeExtend = mapToTargetMachineInstruction constantDeExtendInstr

constantDeExtendInstr mi @ MachineBundle {mbInstrs = mis} =
  mi {mbInstrs = map constantDeExtendInstr mis}
constantDeExtendInstr mi @ MachineSingle {msOpcode = opcode,
                                          msProperties = ps}
  | isConstantExtended (mopcTarget opcode) =
    mi {msOpcode     = liftToTOpc nonConstantExtendedInstr opcode,
        msProperties = ps ++ [mkMachineInstructionPropertyCustom "ce"]}
constantDeExtendInstr mi = mi

removeFrameIndex = mapToTargetMachineInstruction removeFrameIndexInstr

removeFrameIndexInstr
  mi @ MachineSingle {msOpcode = MachineTargetOpc TFR_FI_fi,
                      msOperands = [r,
                                    off @ MachineImm {},
                                    MachineImm {miValue = 0}]} =
  mi {msOpcode = mkMachineTargetOpc A2_addi,
      msOperands = [r, mkMachineRegSP, off]}
removeFrameIndexInstr mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                          msOperands = mops}
  | i `elem` fiInstrs =
      let mopc' = mkMachineTargetOpc $ read $ dropSuffix "_fi" (show i)
          mops' = case mops of
                    [off @ MachineImm {}, MachineImm {miValue = 0},
                     r @ MachineReg {}] -> [mkMachineRegSP, off, r]
                    -- Example: L2_loadri_io_fi [dst, base, off]
                    [r @ MachineReg {},
                     MachineImm {miValue = base}, MachineImm {miValue = off}] ->
                            [r, mkMachineRegSP, mkMachineImm (base + off)]
                    -- TODO: what do we do with the non-offset value? (which is
                    -- non-zero)
                    [off @ MachineImm {}, MachineImm {}, r @ MachineReg {}] ->
                      [mkMachineRegSP, off, r]
                    [p @ MachineReg {}, off @ MachineImm {},
                     MachineImm {miValue = 0}, r @ MachineReg {}] ->
                      [p, mkMachineRegSP, off, r]
                    _ -> error ("unmatched: removeFrameIndexInstr " ++ show mi)
      in mi {msOpcode = mopc', msOperands = mops'}
  | otherwise = mi

mkMachineRegSP = mkMachineReg hexagonSP

normalizeNewValueCmpJump = mapToMachineBlock normalizeNewValueCmpJumpInBlock

normalizeNewValueCmpJumpInBlock mb @ MachineBlock {mbInstructions = mis} =
  case find isCmpComboMachineInstr (concatMap miToList mis) of
   Just MachineSingle {msOpcode = MachineTargetOpc i,
                       msOperands = [_, src1, src2]} ->
     let mb1 = filterMachineInstructionsBlock (not . isCmpComboMachineInstr) mb
         mb2 = concatMapToMachineInstructionBlock
               (normalizeNewValueCmpJumpCombo i [src1, src2]) mb1
     in mb2
   Nothing -> mb

isCmpComboMachineInstr MachineSingle {msOpcode = MachineTargetOpc i} =
  isCmpComboInstr i
isCmpComboMachineInstr _ = False

normalizeNewValueCmpJumpCombo ci mops
  mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = [_, l]}
  | isNewValueCmpJump i =
    [mi {msOpcode = mkMachineTargetOpc $ realNewValueJumpInstr ci i,
         msOperands = mops ++ [l]}]
normalizeNewValueCmpJumpCombo _ _ mi = [mi]

realNewValueJumpInstr cmp jmp =
  read $
  "J4_" ++
  (dropPrefix "CX_" $ dropSuffix "_combo" $ show cmp) ++ "_" ++
  (if isTrueNVCmpJump cmp jmp then "t" else "f") ++
  "_jumpnv_t"

isTrueNVCmpJump cmp jmp
  | cmp `elem` [C2_cmplt_combo, C2_cmpltu_combo] = not (isTrueNVJump jmp)
  | otherwise = isTrueNVJump jmp

isTrueNVJump J4_combo_t_jumpnv_t = True
isTrueNVJump J4_combo_f_jumpnv_t = False

normalizeNVJumps = mapToTargetMachineInstruction normalizeNVJump

normalizeNVJump mi @ MachineSingle {msOpcode = MachineTargetOpc i}
  | isNVJmpInstr i =
      mi {msOpcode = mkMachineTargetOpc (externalNewValueJump i)}
normalizeNVJump mi = mi

externalNewValueJump J2_jumpt_nv = J2_jumptnew
externalNewValueJump J2_jumpf_nv = J2_jumpfnew

normalizeJRInstrs = concatMapToMachineInstruction normalizeJR

normalizeJR mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                msOperands = mops}
  | i == L2_deallocframe_linear =
    [mi {msOpcode = mkMachineTargetOpc L2_deallocframe, msOperands = []}]
  | i == JMPret_dealloc_linear =
      case mops of
       [_, dst, _] -> [mi {msOpcode = mkMachineTargetOpc JMPret,
                           msOperands = [dst]}]
  | i == L4_return_linear =
    [mi {msOpcode = mkMachineTargetOpc L4_return, msOperands = []}]
  | i == JMPret_linear =
      case mops of
       [_, dst] -> [mi {msOpcode = mkMachineTargetOpc JMPret,
                        msOperands = [dst]}]
normalizeJR MachineSingle {msOpcode = MachineTargetOpc i}
  | i `elem` [Ret_dealloc_merge, Jr_merge] = []
normalizeJR ms @ MachineSingle {msOpcode = MachineTargetOpc i}
  | i `elem` [TCRETURNi, TCRETURNi_ce] =
    [ms {msOpcode = mkMachineTargetOpc J2_jump}]
normalizeJR mi = [mi]

expandCondTransfers = mapToMachineBlock (expandBlockPseudos expandCondTransfer)

expandCondTransfer mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                       msOperands = [dst, cond, src1, src2]}
  | isCondTransferInstr i =
    let (_, new) = muxTransferInstr i
        iT       = primitiveCondTransfer src1 True  new
        iF       = primitiveCondTransfer src2 False new
        mi1      = mi {msOpcode = mkMachineTargetOpc iT,
                       msOperands = [dst, cond, src1]}
        mi2      = mi {msOpcode = mkMachineTargetOpc iF,
                       msOperands = [dst, cond, src2]}
    in [[mi1, mi2]]

expandCondTransfer mi = [[mi]]

primitiveCondTransfer MachineReg {} True  False = A2_tfrt
primitiveCondTransfer MachineReg {} True  True  = A2_tfrtnew
primitiveCondTransfer MachineReg {} False False = A2_tfrf
primitiveCondTransfer MachineReg {} False True  = A2_tfrfnew
primitiveCondTransfer MachineImm {} True  False = C2_cmoveit
primitiveCondTransfer MachineImm {} True  True  = C2_cmovenewit
primitiveCondTransfer MachineImm {} False False = C2_cmoveif
primitiveCondTransfer MachineImm {} False True  = C2_cmovenewif

addJumpHints = mapToTargetMachineInstruction addJumpHint

addJumpHint mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                msProperties = mps}
  | isJumpNew i || isNewValueCmpJump i =
    case find isMachineInstructionPropertyBranchTaken mps of
     Just (MachineInstructionPropertyBranchTaken taken) ->
       mi {msOpcode = mkMachineTargetOpc (addHint taken i)}
     Nothing -> mi
addJumpHint mi = mi

addHint True J2_jumptnew = J2_jumptnewpt
addHint True J2_jumpfnew = J2_jumpfnewpt
addHint False i | isNewValueCmpJump i = read (init (show i) ++ "nt")
addHint _ i = i

-- | Gives a list of function transformers
transforms _ ImportPreLift = [liftStackArgSize,
                              peephole extractReturnRegs,
                              peephole foldStackPointerCopy,
                              mapToOperation addAlternativeInstructions]
transforms to AugmentPreRW = [peephole (expandJumps to)]
transforms _ AugmentPostRW = [addCSLoadEffects]
transforms _ ExportPostOffs = [allocateArgArea, alignAllocFrame]
transforms _ ExportPreLow = [shiftFrameOffsets]
transforms _ _ = []

-- | Latency of read-write dependencies

readWriteLatency _ (_, Read) (_, Write) = 0
-- This is to allow dual stores (see 5.5 in "Hexagon V4 Programmer's Reference
-- Manual" and "isLegalToPacketizeTogether" in HexagonVLIWPacketizer.cpp)
readWriteLatency m (_, Write) (_, Write) | isMemoryObject m = 0
-- This is so that linear jumps and merge jumps can be scheduled in parallel
readWriteLatency ControlSideEffect (_, Write) (_, Write) = 0
readWriteLatency _
  ((TargetInstruction p, _), Write) ((TargetInstruction c, _), Write)
  | p `elem` linearMergeJumps && c `elem` linearMergeJumps = 0
readWriteLatency _ ((_, VirtualType (DelimiterType InType)), _) (_, _) = 1
readWriteLatency _ ((_, VirtualType FunType), _) (_, _) = 1
readWriteLatency _ ((_, VirtualType _), _) (_, _) = 0
readWriteLatency _ ((TargetInstruction p, _), _) (_, _) =
    maybeMax 0 $ map occupation (usages [] p)

linearMergeJumps = [JMPret_dealloc_linear, L4_return_linear, Ret_dealloc_merge,
                    JMPret_linear, Jr_merge]

isMemoryObject (Memory _) = True
isMemoryObject AllMemory  = True
isMemoryObject _          = False

-- | Alternative temporaries of each operand

-- All temps that hold the same value

alternativeTemps _ _ _ ts = map fst ts

-- | Copy expansion

expandCopy _ _ o = [o]

-- | Custom processor constraints

constraints to f = forbiddenNewValueConstraints (flatCode f) ++
                   if usePredicateRegsForNvcj to then
                     foldMatch newValueCmpJumpComboConstraints [] f else []

-- new-value instructions (with bypassing operands) cannot use the result
-- of conditional transfers
forbiddenNewValueConstraints fcode =
  concatMap (forbiddenNVForDef fcode) fcode

forbiddenNVForDef fcode
  d @ SingleOperation {oOpr = Natural Linear {
                          oIs = is,
                          oDs = [MOperand {altTemps = [t]}]}} =
    let us = potentialUsers t fcode
    in concat [concatMap (forbiddenNVForCT (oId d) t i) us
              | TargetInstruction i <- is, isCondTransferInstr i]
forbiddenNVForDef _ _ = []

forbiddenNVForCT did t i u =
  concatMap (forbiddenNVForUser did t i u) (oInstructions u)

forbiddenNVForUser did t i u (TargetInstruction i') =
  let pbs = [p | (p, True) <- tempBypasses (operandInfo []) u i',
             t `elem` extractTemps p]
  in [forbiddenNewValueConstraint (operandId p) (tId t) did i (oId u) i'
     | p <- pbs]
forbiddenNVForUser _ _ _ _ _ = []

forbiddenNewValueConstraint pid tid did di uid ui =
  NotExpr (AndExpr
           [ConnectsExpr pid tid,
            ImplementsExpr did (TargetInstruction di),
            ImplementsExpr uid (TargetInstruction ui)])

newValueCmpJumpComboConstraints (
  c @ SingleOperation {oOpr = Natural (Linear {
                                          oIs = is,
                                          oDs = [MOperand {altTemps = [d]}]})}
  :
  code) constraints
  | any isCmpComboTargetInstr is =
      case find (\j -> isBranch j && isPotentialUser d j) code of
       Just j ->
         let ci = fromJust $ find isCmpComboTargetInstr is
             ce = ImplementsExpr (oId c) ci
             ji = fromJust $ find isJumpComboTargetInstr (oInstructions j)
             je = ImplementsExpr (oId j) ji
             cc = AndExpr [ImpliesExpr ce je, ImpliesExpr je ce]
         in (code, constraints ++ [cc])
       Nothing -> (code, constraints)

newValueCmpJumpComboConstraints (_ : code) constraints = (code, constraints)

isCmpComboTargetInstr (TargetInstruction i) = isCmpComboInstr i
isCmpComboTargetInstr _ = False

isJumpComboTargetInstr (TargetInstruction i) = isComboNVCJ i
isJumpComboTargetInstr _ = False
