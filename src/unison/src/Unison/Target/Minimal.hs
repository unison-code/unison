{-|
Copyright   :  Copyright (c) 2020, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.Minimal (target) where

import qualified Data.Set as S
import Data.Maybe
import qualified Data.Map as M

import MachineIR hiding (parse)

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray

import Unison.Target.Minimal.MinimalRegisterDecl
import Unison.Target.Minimal.MinimalResourceDecl
import Unison.Target.Minimal.SpecsGen.MinimalInstructionDecl
import Unison.Target.Minimal.SpecsGen.MinimalRegisterClassDecl
import qualified Unison.Target.Minimal.SpecsGen as SpecsGen

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
      API.tRematInstrs      = const rematInstrs,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = const operandInfo,
      API.tAlignedPairs     = const alignedPairs,
      API.tPackedPairs      = const packedPairs,
      API.tRelatedPairs     = const relatedPairs,
      -- Pass target options to 'resources' to specify target width
      API.tResources        = resources,
      API.tUsages           = const usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = const readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy,
      API.tConstraints      = const constraints,
      API.tSpillOverhead    = const spillOverhead
    }

instance Read MinimalInstruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

-- | Register array
registerArray = [RegisterClass R32, InfiniteRegisterClass M32]

-- | Register atoms
registerAtoms ra = (ra, ra)

-- | Register classes
regClasses = [RegisterClass R32, InfiniteRegisterClass M32]

-- | Individual registers of each register class
registers (RegisterClass R32) = [R0, R1, R2, R3, R4, R5, R6, R7]

-- | Map from infinite register class to register usage
infRegClassUsage (InfiniteRegisterClass M32) = 1

-- | Map from infinite register class to (possibly) register atom upper bound
infRegClassBound = const Nothing

-- | Index type (low/high/copy) of subregisters
subRegIndexType _ subreg = error ("unmatched: subRegIndexType " ++ show subreg)

-- | Registers that are not preserved across calls
callerSaved = [R0, R1, R2, R3]

-- | Registers that are preserved across calls
calleeSaved = [R4, R5, R6]

-- | Registers whose value cannot be moved around
reserved = [R7]

instance Read MinimalRegister where
  readsPrec _ s = [(readReg s, "")]

readReg s = case M.lookup s (inverseMap regStrings) of
              (Just r) -> r
              Nothing -> error $ "unmatched: readReg " ++ s

instance Show MinimalRegister where
  show r = case M.lookup r regStrings of
             (Just s) -> s
             Nothing -> error $ "unmatched: show MinimalRegister"

regStrings = M.fromList $
  [(R0, "r0"), (R1, "r1"), (R2, "r2"), (R3, "r3"),
   (R4, "r4"), (R5, "r5"), (R6, "r6"), (R7, "r7")]

-- | Gives the type of natural instruction according to the operation
instructionType = SpecsGen.instructionType

-- | Gives the target of a branch instruction and the type of branch
branchInfo (Branch {oBranchIs = is, oBranchUs = [_, BlockRef l]})
  | targetInst is `elem` [Bif] = BranchInfo Conditional (Just l)
branchInfo (Branch {oBranchIs = is, oBranchUs = [BlockRef l]})
  | targetInst is `elem` [B]   = BranchInfo Unconditional (Just l)
branchInfo (Branch {oBranchIs = is})
  | targetInst is `elem` [Br]  = BranchInfo Unconditional Nothing
branchInfo o = error ("unmatched pattern: branchInfo " ++ show (mkSingleOperation (-1) (Natural o)))

-- | Target dependent MIR pre-processing functions
preProcess = []

-- | Target dependent MIR post-processing functions
postProcess = []

-- | Gives a list of function transformers
transforms _ = []

-- | Set of def copies and list of sets of use copies to extend a temporary

-- Do not extend temporaries pre-allocated to reserved registers
copies _fInfo _phiTemp _t [r] _d us | r `elem` reserved =
    ([], replicate (length us) [])
-- Add only one store for entry calle-saved temporaries
-- Add only one load for exit calle-saved temporaries
-- Do not add copies for intermediate calle-saved temporaries
copies (f, cst, _, _, _, _) False t [_] _d [_u]
  | S.member t cst =
    (
      if isEntryTemp (fCode f) t
      then [mkNullInstruction, TargetInstruction Store]
      else [],
      [if isExitTemp (fCode f) t
       then [mkNullInstruction, TargetInstruction Load]
       else []]
    )
copies _ _ _ _ _d us = (defCopies, replicate (length us) useCopies)

defCopies = [mkNullInstruction] ++ map TargetInstruction [Move, Store]
useCopies = [mkNullInstruction] ++ map TargetInstruction [Move, Load]

-- | Gives rematerialization instruction triples of an instruction
rematInstrs _ = Nothing

-- | Transforms copy instructions into natural instructions
fromCopy o @ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [Move] = toLinear o
  | i `elem` [Store] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs  = [s, mkSP, mkBoundMachineFrameObject d],
            oDs  = []}
  | i `elem` [Load] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs  = [mkSP, mkBoundMachineFrameObject s],
            oDs  = [d]}
fromCopy (Natural o) = o
fromCopy o = error ("unmatched pattern: fromCopy " ++ show o)

mkSP = Register $ mkTargetRegister R0

mkBoundMachineFrameObject (Register r) =
  mkBound (mkMachineFrameObject (infRegPlace r) (Just 4) 4 False)

fromCopyInstr = fromJust . SpecsGen.parent

-- | Gives information about the operands of each instruction
operandInfo i = SpecsGen.operandInfo i

-- | Gives pairs of aligned operands in each instruction
alignedPairs i os = SpecsGen.alignedPairs i os

-- | Gives pairs of packed operands in each instruction
packedPairs _ _ = []

-- | Gives pairs of operands whose registers are related extensionally
relatedPairs _ = []

-- | Declares target architecture resources. An issue width of w can be
-- specified with the option --targetoption=issue-width:w.
resources to =
  let w = read $ fromMaybe "1" (API.optionValue "issue-width" to)
  in
    [
      -- Resource used for code size minimization
      Resource BundleWidth w,
      -- Issue width (single issue)
      Resource Issue w
    ]

-- | Gives usages of the processor resources by each instruction
usages i = [mkUsage Issue 1 1, mkUsage BundleWidth (SpecsGen.size i) 1]

-- | No-operation instruction
nop = Linear [TargetInstruction Nop] [] []

-- | Side-effects of each instruction
readWriteInfo = SpecsGen.readWriteInfo

-- | Implementation of frame setup and destroy operations
implementFrame = const []

-- | Adds function prologue
addPrologue _ code = code

-- | Adds function epilogue
addEpilogue _ code = code

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Latency of read-write dependencies
readWriteLatency _ (_, Read) (_, Write) = 0
readWriteLatency _ ((_, VirtualType (DelimiterType InType)), _) (_, _) = 1
readWriteLatency _ ((_, VirtualType FunType), _) (_, _) = 1
readWriteLatency _ ((_, VirtualType _), _) (_, _) = 0
readWriteLatency _ ((TargetInstruction p, _), _) (_, _) =
    maybeMax 0 $ map occupation (usages p)

-- | Alternative temporaries of each operand
alternativeTemps _ _ _ ts = map fst ts

-- | Copy expansion
expandCopy _ _ o = [o]

-- | Custom processor constraints
constraints _ = []

-- | Spill sign and overhead of each instruction and operands
spillOverhead _ = Nothing
