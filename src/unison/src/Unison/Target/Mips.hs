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
module Unison.Target.Mips (target) where

import qualified Data.Set as S
import Data.Maybe
import Data.List.Split

import MachineIR hiding (parse)

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Analysis.TemporaryType
import Unison.Analysis.TransitiveOperations
import Unison.Transformations.FoldReservedRegisters

import Unison.Target.Mips.Registers
import Unison.Target.Mips.Transforms
import Unison.Target.Mips.MipsRegisterDecl
import Unison.Target.Mips.MipsRegisterClassDecl
import Unison.Target.Mips.SpecsGen.MipsInstructionDecl
import qualified Unison.Target.Mips.SpecsGen as SpecsGen

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
      API.tInstructionType  = const SpecsGen.instructionType,
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

instance Read MipsInstruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

-- | Gives the type of natural instruction according to the operation

-- | Gives the target of a jump instruction and the type of jump

branchInfo (Branch {oBranchIs = ops, oBranchUs = [_, BlockRef l]})
  | targetInst ops `elem` [BGEZ, BLTZ, BGTZ, BLEZ, BEQZC, BEQZALC, BNEZC, BNEZALC, BC1F, BC1T] =
    BranchInfo Conditional (Just l)
branchInfo (Branch {oBranchIs = ops, oBranchUs = [_, _, BlockRef l]})
  | targetInst ops `elem` [BEQ, BNE] = BranchInfo Conditional (Just l)
branchInfo (Branch {oBranchIs = [TargetInstruction B], oBranchUs = [BlockRef l]}) =
    BranchInfo Unconditional (Just l)
branchInfo (Branch {oBranchIs = [TargetInstruction JR]}) =
    BranchInfo Unconditional Nothing
branchInfo (Branch {oBranchIs = [TargetInstruction RetRA]}) =
    BranchInfo Unconditional Nothing
branchInfo (Branch {oBranchIs = [TargetInstruction PseudoIndirectBranch]}) =
    BranchInfo Unconditional Nothing
branchInfo (Branch {oBranchIs = [TargetInstruction PseudoReturn]}) =
    BranchInfo Unconditional Nothing

-- | Gives a set of def copies and a list of sets of use copies to extend
-- the given temporary

-- Do not extend temporaries that are congruent to temporaries pre-allocated
-- to reserved registers
copies _fInfo _phiTemp _t [r] _d us | isReservedRegister r =
    ([], replicate (length us) [])

-- Do not extend temporaries that are defined by virtual defines
copies _fInfo False _t _rs d [_] | isDefine d = ([], [[]])

-- Do not extend temporaries that are only used by virtual kills
copies _fInfo False _t _rs _d [u] | isKill u = ([], [[]])

-- Add only one store for entry calle-saved temporaries
-- Add only one load for exit calle-saved temporaries
-- Do not add copies for intermediate calle-saved temporaries
copies (f, cst, _, _, _, _) False t [r] _d [_u]
  | S.member t cst =
    (
      if isEntryTemp (fCode f) t
      then [mkNullInstruction, TargetInstruction (pushInstruction r)]
      else [],
      [if isExitTemp (fCode f) t
       then [mkNullInstruction, TargetInstruction (popInstruction r)]
       else []]
    )

-- Extend temporaries defined in acc64 with mflo and mfhi only
copies _ False _ [] d us
    | isNatural d && targetInst (oInstructions d) `elem` [MULT, MULTu, DIV, MADD] =
      ([], replicate (length us) [])
copies _ False t [] d us
    | isLow d  = ([mkNullInstruction, TargetInstruction MFLO], map (accCopy t) us)
    | isHigh d = ([mkNullInstruction, TargetInstruction MFHI], map (accCopy t) us)

-- Extend temporaries combined to be used by a macc with mtlo and mthi
copies _ False t [] _ [u] | isCombine u = (accCopy t u, [[]])
copies _ False _ [] d us | isCombine d =
  ([], replicate (length us) [])

copies (f, _, cg, ra, bcfg, sg) _ t _ d us =
  let is = d:us
      w  = widthOfTemp ra cg f t is
      -- This below is just to determine which temporaries require only 32-bits
      -- FP rather than GP copies
      drcs = transitiveRegClasses operandInfo bcfg sg Reaching f t
      dors = transitivePreAssignments bcfg sg Reaching f t
      urcs = transitiveRegClasses operandInfo bcfg sg Reachable f t
      uors = transitivePreAssignments bcfg sg Reachable f t
      (fpds, fpus) = (all isFGR32Class drcs && all isFGR32 dors,
                      all isFGR32Class urcs && all isFGR32 uors)
  in ((defCopies fpds w d),
      map (useCopies fpus w) us)

defCopies False 1 _ = [mkNullInstruction] ++ map TargetInstruction [MOVE, STORE]
defCopies True  1 _ = [mkNullInstruction] ++ map TargetInstruction [MOVE_F, STORE_F]
defCopies _     2 _ = [mkNullInstruction] ++ map TargetInstruction [MOVE_D, STORE_D]

useCopies False 1 _ = [mkNullInstruction] ++ map TargetInstruction [MOVE, LOAD]
useCopies True  1 _ = [mkNullInstruction] ++ map TargetInstruction [MOVE_F, LOAD_F]
useCopies _     2 _ = [mkNullInstruction] ++ map TargetInstruction [MOVE_D, LOAD_D]

widthOfTemp = widthOf (target, [])

isFGR32Class rc = rc `elem` map RegisterClass [FGR32, FGR32Opnd]

isFGR32 r = mipsReg r `elem` registers (RegisterClass FGR32Opnd)

mipsReg = rTargetReg . regId

accCopy t i
  | isCombine i && isCombineLowOf t i  = [mkNullInstruction, TargetInstruction MTLO]
  | isCombine i && isCombineHighOf t i = [mkNullInstruction, TargetInstruction MTHI]
  | otherwise = []

pushInstruction r
  | r `elem` registers (RegisterClass GPR32Opnd) = STORE
  | r `elem` registers (RegisterClass AFGR64Opnd) = STORE_D

popInstruction r
  | r `elem` registers (RegisterClass GPR32Opnd) = LOAD
  | r `elem` registers (RegisterClass AFGR64Opnd) = LOAD_D

-- | Transforms copy instructions into natural instructions

fromCopy o @ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MOVE, MFLO, MFHI, MTLO, MTHI, MOVE_D] = toLinear o
  | i `elem` [STORE, STORE_F, STORE_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs  = [s, mkOprMipsSP, mkBoundMachineFrameObject i d],
            oDs  = []}
  | i `elem` [LOAD, LOAD_F, LOAD_D] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs  = [mkOprMipsSP, mkBoundMachineFrameObject i s],
            oDs  = [d]}

fromCopy o = error ("unmatched pattern: fromCopy " ++ show o)

mkOprMipsSP = Register $ mkTargetRegister SP

mkBoundMachineFrameObject i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size)

stackSize op
  | op `elem` [STORE, STORE_F, LOAD, LOAD_F] = 4
  | op `elem` [STORE_D, LOAD_D] = 8

fromCopyInstr = fromJust . SpecsGen.parent

-- | Tells whether the given register can be moved across register spaces

isReservedRegister ZERO = True
isReservedRegister _ = False

-- | Declares target architecture resources

data MipsResource = BundleWidth deriving (Eq, Ord, Show, Read)

resources = [Resource BundleWidth 1]

-- | Declares resource usages of each instruction

usages = const [mkUsage BundleWidth 1 1]

-- | No-operation instruction

nop = Linear [TargetInstruction NOP] [] []

-- | Implementation of frame setup and destroy operations

implementFrame = const []

mkOpt oid i us ds =
  let o  = mkLinear oid [mkNullInstruction, TargetInstruction i] us ds
      o' = addActivators (map TargetInstruction spUsers) o
  in o'

addActivators = mapToActivators . (++)

-- | Adds function prologue

addPrologue oid (e:code) =
  let addNegSp = mkOpt oid ADDiu_negsp [Bound mkMachineFrameSize] []
  in [e, addNegSp] ++ code

-- | Adds function epilogue

addEpilogue oid code =
    let [f, e] = split (keepDelimsL $ whenElt isBranch) code
        addSp = mkOpt oid ADDiu_sp [Bound mkMachineFrameSize] []
    in f ++ [addSp] ++ e

spUsers = filter isSPUser SpecsGen.allInstructions
isSPUser = readsObject (OtherSideEffect SP)
readsObject rwo i = rwo `elem` (fst $ SpecsGen.readWriteInfo i)

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess = [addFrameIndex, serializeDelaySlots]

addFrameIndex = mapToTargetMachineInstruction addFrameIndexInstr

addFrameIndexInstr mi @ MachineSingle {msOpcode = opcode,
                                       msOperands = operands}
  | any isMachineFrameIndex operands &&
    any isTemporaryInfo (fst $ operandInfo $ mopcTarget opcode) =
      mi {msOpcode = liftToTOpc (\i -> read (show i ++ "_fi")) opcode}
  | otherwise = mi

liftToTOpc f = mkMachineTargetOpc . f . mopcTarget

serializeDelaySlots = mapToMachineBlock serializeDelaySlotInBlock

serializeDelaySlotInBlock mb @ MachineBlock {mbInstructions = mis} =
  mb {mbInstructions = concatMap serializeDelaySlot mis}

serializeDelaySlot mi @ MachineSingle {} = [mi]
serializeDelaySlot MachineBundle {mbInstrs = mis} = mis

-- | Target dependent post-processing functions

postProcess = [expandPseudos]

expandPseudos = mapToMachineBlock (expandBlockPseudos expandPseudo)

expandPseudo mi @ MachineSingle {msOpcode   = MachineTargetOpc i,
                                 msOperands = mos} =
  [[expandSimple mi (i, mos)]]
expandPseudo mi = [[mi]]

expandSimple mi (RetRA, _) =
  mi {msOpcode = MachineTargetOpc PseudoReturn,
      msOperands = [mkMachineReg RA]}

expandSimple mi (i, [MachineImm mfs])
  | i `elem` [ADDiu_sp, ADDiu_negsp] =
      let mfs' = if i == ADDiu_negsp then (-mfs) else mfs
      in  mi {msOpcode   = MachineTargetOpc ADDiu,
              msOperands = [mkMachineReg SP, mkMachineReg SP,
                            mkMachineImm mfs']}

expandSimple mi (i, [v, off])
  | i `elem` [SW_sp, SWC1_sp] =
    mi {msOpcode   = MachineTargetOpc (fromJust $ SpecsGen.parent i),
        msOperands = [v, mkMachineReg SP, off]}

expandSimple mi (MOVE, [d, s]) =
  mi {msOpcode   = MachineTargetOpc OR,
      msOperands = [d, s, mkMachineReg ZERO]}

expandSimple mi _ = mi

-- | Gives a list of function transformers
transforms ImportPreLift = [peephole rs2ts,
                            peephole normalizeCallPrologue,
                            peephole normalizeCallEpilogue,
                            peephole extractReturnRegs,
                            (\f -> foldReservedRegisters f (target, [])),
                            mapToOperation hideStackPointer]
transforms _ = []

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

operandInfo i
    | i `elem` [LW, LH, LBu, LB, LHu] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0, BoundInfo],
         [TemporaryInfo (RegisterClass GPR32Opnd) 1])
    | i `elem` [LWL, LWR] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0, BoundInfo,
          TemporaryInfo (RegisterClass GPR32Opnd) 0],
         [TemporaryInfo (RegisterClass GPR32Opnd) 1])
    | i `elem` [SW, SB, SH, SWL, SWR] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0, BoundInfo],
         [])
    | i `elem` [LDC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0, BoundInfo],
         [TemporaryInfo (RegisterClass AFGR64Opnd) 1])
    | i `elem` [SWC1] =
        ([TemporaryInfo (RegisterClass FGR32Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0,
          BoundInfo],
         [])
    | i `elem` [SDC1] =
        ([TemporaryInfo (RegisterClass AFGR64Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0,
          BoundInfo],
         [])
    | i `elem` [LWC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0, BoundInfo],
         [TemporaryInfo (RegisterClass FGR32Opnd) 1])
    | i `elem` [LWXC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0],
         [TemporaryInfo (RegisterClass FGR32Opnd) 1])
    | i `elem` [LDXC1] =
        ([TemporaryInfo (RegisterClass GPR32Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0],
         [TemporaryInfo (RegisterClass AFGR64Opnd) 1])
    | i `elem` [SWXC1] =
        ([TemporaryInfo (RegisterClass FGR32Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0,
          TemporaryInfo (RegisterClass GPR32Opnd) 0],
         [])
operandInfo a = SpecsGen.operandInfo a
