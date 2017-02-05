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
import Unison.Target.Hexagon.Registers
import Unison.Target.Hexagon.Transforms
import Unison.Target.Hexagon.Common
import Unison.Target.Hexagon.HexagonRegisterDecl
import Unison.Target.Hexagon.HexagonRegisterClassDecl
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl
import Unison.Target.Hexagon.SpecsGen.HexagonItineraryDecl
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
      API.tPreProcess       = const preProcess,
      API.tPostProcess      = const postProcess,
      API.tTransforms       = const transforms,
      API.tCopies           = const copies,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = const operandInfo,
      API.tAlignedPairs     = const SpecsGen.alignedPairs,
      API.tPackedPairs      = const (const (const [])),
      API.tResources        = const resources,
      API.tUsages           = const usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = const readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy
    }

constantExtendedInstr :: HexagonInstruction -> HexagonInstruction
constantExtendedInstr i = read $ show i ++ "_ce"

nonConstantExtendedInstr :: HexagonInstruction -> HexagonInstruction
nonConstantExtendedInstr i = read $ dropSuffix "_ce" (show i)

isConstantExtended :: HexagonInstruction -> Bool
isConstantExtended i = "_ce" `isSuffixOf` (show i)

-- | Gives the type of natural operation according to the instruction

instructionType i
    | i `elem` [MVW, MVD, STW, STD, LDW, LDD, MVPR, MVRP, STW_nv] =
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
       then [mkNullInstruction, TargetInstruction (storeOp w)]
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

moveOp 1 = MVW
moveOp 2 = MVD

storeOp 1 = STW
storeOp 2 = STD

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

-- | Transforms copy instructions into natural instructions

fromCopy Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MVW, MVD, MVPR, MVRP] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [s],
            oDs = [d]}
  | i `elem` [STW, STD, STW_nv] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [mkOprHexagonSP, mkBoundMachineFrameObject i d, s],
            oDs = []}
  | i `elem` [LDW, LDD] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [mkOprHexagonSP, mkBoundMachineFrameObject i s],
            oDs = [d]}

fromCopy i = error ("unmatched pattern: fromCopy " ++ show i)

mkOprHexagonSP = Register $ mkTargetRegister hexagonSP

mkBoundMachineFrameObject i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size)

stackSize op
  | op `elem` [STW, STW_nv, LDW] = 4
  | op `elem` [STD, LDD] = 8

fromCopyInstr i
  | isJust (SpecsGen.parent i) = fromJust (SpecsGen.parent i)
fromCopyInstr STW = S2_storeri_io
fromCopyInstr STD = S2_storerd_io
fromCopyInstr STW_nv = S2_storerinew_io
fromCopyInstr LDW = L2_loadri_io
fromCopyInstr LDD = L2_loadrd_io


-- | Gives information about the operands of each instruction

-- TODO: this is terrible! we need to respecify the operand info of all
-- instructions with use latency -1... can we do this better? (e.g. via custom
-- Haskell functions specified in hexagon-manual.yaml).
operandInfo i
  | i `elem`
      [J2_jumpfnew, J2_jumpfnewpt, J2_jumptnew, J2_jumptnewpt]
    = ([TemporaryInfo (RegisterClass F32) (-1), BlockRefInfo], [])
  | i `elem` [A2_tfrt, A2_tfrf] =
    ([TemporaryInfo (RegisterClass PredRegs) (-1),
      TemporaryInfo (RegisterClass IntRegs) 0],
     [TemporaryInfo (RegisterClass IntRegs) 1])
  -- New-value stores (the last use of class IntRegs has latency -1)
  | baseInstr i `elem` [S2_storerinew_io, S2_storerbnew_io, S2_storerhnew_io] =
    ([TemporaryInfo (RegisterClass IntRegs) 0, BoundInfo,
      TemporaryInfo (RegisterClass IntRegs) (-1)],
     [])
  | baseInstr i `elem` [S2_storerbnew_pi, S2_storerhnew_pi, S2_storerinew_pi] =
    ([TemporaryInfo (RegisterClass IntRegs) 0, BoundInfo,
      TemporaryInfo (RegisterClass IntRegs) (-1)],
     [TemporaryInfo (RegisterClass IntRegs) 1])
  | baseInstr i `elem` [S2_storerinewabs] =
    ([BoundInfo, TemporaryInfo (RegisterClass IntRegs) (-1)], [])
  | baseInstr i `elem` [S2_storerinew_io_fi] =
    ([BoundInfo, BoundInfo, TemporaryInfo (RegisterClass IntRegs) (-1)], [])
  | i `elem` [S4_storerhnew_rr, S4_storerinew_rr] =
    ([TemporaryInfo (RegisterClass IntRegs) 0,
      TemporaryInfo (RegisterClass IntRegs) 0, BoundInfo,
      TemporaryInfo (RegisterClass IntRegs) (-1)],
     [])
  | otherwise = SpecsGen.operandInfo i

baseInstr i
  | isConstantExtended i = nonConstantExtendedInstr i
  | otherwise = i

-- | Declares target architecture resources

data HexagonResource =
  BundleWidth |
  Slot0123 |
  Slot01 |
  Slot0 |
  Slot23 |
  Slot2 |
  Slot3 |
  Store |
  Nvcmp |
  BlockEnd
  deriving (Eq, Ord, Show, Read)

resources =
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

     -- Artificial resource to represent the fact that there can
     -- only be up to two regular stores or a single new-value store per bundle
     Resource Store 2,    -- Used by ST and NVST

     -- Artificial resource to disallow jump merges and ENDLOOP instructions to
     -- be scheduled together with (out)-delimiters
     Resource BlockEnd 1

    ]

-- | Declares resource usages of each instruction

usages i
  | i `elem` [Jump_merge, Jr_merge] = [mkUsage BlockEnd 1 1]
usages i
  | isConstantExtended i =
    let it = SpecsGen.itinerary (nonConstantExtendedInstr i)
    in mergeUsages (itineraryUsage i it) [mkUsage BundleWidth 1 1]
  | otherwise = itineraryUsage i $ SpecsGen.itinerary i

itineraryUsage i it
    | it `elem` [ALU32_2op_tc_1_SLOT0123, ALU32_2op_tc_2early_SLOT0123,
                 ALU32_3op_tc_1_SLOT0123, ALU32_3op_tc_2early_SLOT0123,
                 ALU32_ADDI_tc_1_SLOT0123, EXTENDER_tc_1_SLOT0123, PSEUDO] =
        [mkUsage BundleWidth 1 1, mkUsage Slot0123 1 1]
    | it `elem` [ALU64_tc_1_SLOT23, ALU64_tc_2_SLOT23, ALU64_tc_3x_SLOT23,
                 J_tc_2early_SLOT23, M_tc_2_SLOT23, M_tc_3x_SLOT23,
                 S_2op_tc_1_SLOT23, S_2op_tc_2_SLOT23, S_2op_tc_2early_SLOT23,
                 S_3op_tc_1_SLOT23, S_3op_tc_2_SLOT23, S_3op_tc_2early_SLOT23,
                 CR_tc_2early_SLOT23, ALU64_tc_2early_SLOT23] =
      itineraryUsage i ALU32_2op_tc_1_SLOT0123 ++ [mkUsage Slot23 1 1]

    | it `elem` [CR_tc_2early_SLOT3, CR_tc_3x_SLOT3] =
      itineraryUsage i ALU64_tc_1_SLOT23 ++ [mkUsage Slot3 1 1]
    | it `elem` [J_tc_2early_SLOT2] =
      itineraryUsage i ALU64_tc_1_SLOT23 ++ [mkUsage Slot2 1 1]
    | it `elem` [LD_tc_ld_SLOT01, V2LDST_tc_ld_SLOT01, V4LDST_tc_ld_SLOT01] =
      itineraryUsage i ALU32_2op_tc_1_SLOT0123 ++ [mkUsage Slot01 1 1]
    | it `elem` [ST_tc_st_SLOT01, V2LDST_tc_st_SLOT01, V4LDST_tc_st_SLOT01] =
      itineraryUsage i ALU32_2op_tc_1_SLOT0123 ++
      [mkUsage Slot01 1 1, mkUsage Store 1 1]
    | it `elem` [NCJ_tc_3or4stall_SLOT0] && (mayStore i || i == STW_nv) =
      itineraryUsage i LD_tc_ld_SLOT01 ++
      [mkUsage Slot0 1 1, mkUsage Store 2 1]
      -- A new-value compare and jump instruction i cannot be issued in parallel
      -- with stores as slot 0 will be occupied by i and slot 1 will be occupied
      -- by the instruction feeding i. We model this by saturating the 'Store'
      -- resource.
    | it `elem` [NCJ_tc_3or4stall_SLOT0] && (isLinearNewValueCmpJump i) =
      itineraryUsage i LD_tc_ld_SLOT01 ++
      [mkUsage Slot0 1 1, mkUsage Store 2 1]
    | it `elem` [LD_tc_ld_SLOT0,  ST_tc_3stall_SLOT0, V4LDST_tc_st_SLOT0,
                 NCJ_tc_3or4stall_SLOT0, LD_tc_3or4stall_SLOT0] =
      itineraryUsage i LD_tc_ld_SLOT01 ++ [mkUsage Slot0 1 1]
    | it `elem` [ST_tc_st_SLOT0, ST_tc_ld_SLOT0, V2LDST_tc_st_SLOT0] =
      itineraryUsage i LD_tc_ld_SLOT01 ++
      [mkUsage Slot0 1 1, mkUsage Store 1 1]
      -- ENDLOOP instructions are encoded in the bits 14:15 of the preceeding
      -- instruction in the bundle
    | it `elem` [J_tc_2early_SLOT0123] = [mkUsage BlockEnd 1 1]
    | it `elem` [NoItinerary] = []

itineraryUsage _ it = error ("unmatched: itineraryUsage " ++ show it)

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
   [code'] ->
     -- TODO: check if this is the correct handling (see gcc.xexit.xexit.uni and
     -- gobmk.interface.init_gnugo)
     let [f, o] = split (keepDelimsL $ whenElt isOut) code'
         df     = mkOpt oid L2_deallocframe [] []
     in f ++ [df] ++ o


mkOpt oid inst us ds =
  makeOptional $ mkLinear oid [TargetInstruction inst] us ds

mkAct = addActivators (map TargetInstruction stackAccessors)

addActivators = mapToActivators . (++)

-- We need a stack frame iff:
stackAccessors =
  -- there are function calls,
  [J2_call] ++
  -- there are instructions referring to frame objects, or
  fiInstrs ++
  -- there are spills.
  [STW, STD, STW_nv, LDW, LDD]

fiInstrs = filter (\i -> "_fi" `isSuffixOf` (show i)) SpecsGen.allInstructions

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess = [foldSPCopies, addFrameIndex, constantExtend]

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
    any isTemporaryInfo (fst $ operandInfo i) =
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

-- | Target dependent post-processing functions

postProcess = [constantDeExtend, removeFrameIndex, normalizeJumpMerges,
               normalizeNVJumps, normalizeJRInstrs, addJumpHints,
               flip addImplicitRegs (target, [])]

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
                    [r @ MachineReg {},
                     off @ MachineImm {}, MachineImm {miValue = 0}] ->
                            [r, mkMachineRegSP, off]
                    -- TODO: what do we do with the non-offset value? (which is
                    -- non-zero)
                    [off @ MachineImm {}, MachineImm {}, r @ MachineReg {}] ->
                      [mkMachineRegSP, off, r]
                    [r @ MachineReg {}, off @ MachineImm {}, MachineImm {}] ->
                      [r, mkMachineRegSP, off]
                    _ -> error ("unmatched: removeFrameIndexInstr " ++ show i)
      in mi {msOpcode = mopc', msOperands = mops'}
  | otherwise = mi

mkMachineRegSP = mkMachineReg hexagonSP

normalizeJumpMerges = mapToMachineBlock normalizeJumpMergesInBlock

normalizeJumpMergesInBlock mb @ MachineBlock {mbInstructions = mis} =
  case find isJumpMerge (concatMap miToList mis) of
   Just MachineSingle {msOperands = [_, l], msProperties = mps} ->
     let mb1 = filterMachineInstructionsBlock (not . isJumpMerge) mb
         mb2 = concatMapToMachineInstructionBlock
               (normalizeLinearJump mps l) mb1
     in mb2
   _ -> mb

isJumpMerge MachineSingle {msOpcode = MachineTargetOpc Jump_merge} = True
isJumpMerge _ = False

normalizeLinearJump mps l mi @ MachineSingle {msOpcode = MachineTargetOpc i,
                                              msOperands = (_:mops)}
  | isLinearNewValueCmpJump i || isLinearJump i =
    [mi {msOpcode = mkMachineTargetOpc $ branchJump i,
         msOperands = mops ++ [l],
         msProperties = mps}]
normalizeLinearJump _ _ mi = [mi]

branchJump jmp = read $ dropSuffix "_linear" (show jmp)

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
normalizeJR mi = [mi]

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

mayStore i =
  let ws  = snd (SpecsGen.readWriteInfo i) :: [RWObject HexagonRegister]
      mem = Memory "mem" :: RWObject HexagonRegister
  in mem `elem` ws

-- | Gives a list of function transformers
transforms ImportPreLift = [peephole extractReturnRegs,
                            peephole foldStackPointerCopy,
                            mapToOperation addAlternativeInstructions]
transforms AugmentPreRW = [peephole expandJumps]
transforms AugmentPostRW = [mapToOperation addControlBarrier]
transforms _ = []

-- | Latency of read-write dependencies

readWriteLatency _ (_, Read) (_, Write) = 0
-- This is so that linear jumps and merge jumps can be scheduled in parallel
readWriteLatency ControlSideEffect (_, Write) (_, Write) = 0
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
