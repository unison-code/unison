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
{-# LANGUAGE ExistentialQuantification #-}
module Unison.Target.API (
  RegisterArray(..),
  StackDirection(..),
  Any(..),
  FunctionInfo,
  ReadWriteLatencyFunction,
  OperandInfoFunction,
  AlignedPairsFunction,
  PackedPairsFunction,
  RelatedPairsFunction,
  CopyInstructions,
  TargetDescription(..),
  TargetOptions,
  optionValue,
  isBoolOption,
  TargetWithOptions,
  registerArray,
  registerAtoms,
  regClasses,
  registers,
  infRegClassUsage,
  infRegClassBound,
  subRegIndexType,
  callerSaved,
  calleeSaved,
  reserved,
  instructionType,
  branchInfo,
  preProcess,
  postProcess,
  transforms,
  copies,
  rematInstrs,
  fromCopy,
  operandInfo,
  alignedPairs,
  packedPairs,
  relatedPairs,
  resources,
  usages,
  nop,
  readWriteInfo,
  implementFrame,
  addPrologue,
  addEpilogue,
  stackDirection,
  readWriteLatency,
  alternativeTemps,
  expandCopy,
  constraints
  ) where

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Unison.Base
import Unison.Util
import Unison.Predicates

import MachineIR

-- | Unified representation of the target's register file and other
-- memory locations. It is typically built by the function
-- 'mkRegisterArray'.

data RegisterArray r rc = RegisterArray {
      -- | Register classes related to the array
      raRcs       :: [RegisterClass rc],
      -- | Registers of the given register class
      raRegisters :: RegisterClass rc -> [Operand r],
      -- | Number of register atoms used by each register in the given
      -- register class
      raRcUsage   :: RegisterClass rc -> Integer,
      -- | Indexed register class corresponding to the given register class
      raIndexedRc :: RegisterClass rc -> IndexedRegisterClass rc,
      -- | Sequence of atomic registers that form the register array
      regArray    :: [Operand r],
      -- | Map from registers to register atoms
      regAtoms    :: M.Map (Operand r) [RegisterAtom],
      -- | Register atoms of the given register class
      rcAtoms     :: RegisterClass rc -> [RegisterAtom],
      -- | Register spaces related to the array
      raRss       :: [RegisterSpace],
      -- | First and last register atoms in the given register space
      rsAtoms     :: RegisterSpace -> (RegisterAtom, RegisterAtom),
      -- | Register space corresponding to the given register class
      raRcSpace   :: RegisterClass rc -> RegisterSpace
}

-- | Direction in which the stack grows.

data StackDirection =
  -- | Adding an object to the stack increases the stack address
  StackGrowsUp |
  -- | Adding an object to the stack decreases the stack address
  StackGrowsDown
  deriving (Eq, Show)

optionValue option to =
    case find (\o -> option `isPrefixOf` o) to of
      (Just val) -> Just $ tail $ dropWhile (\c -> c /= ':') val
      Nothing    -> Nothing

isBoolOption option to = option `elem` to

-- | Function that possibly gives the latency of a 'RWObject'
-- dependency between a source and a destination instruction.
type ReadWriteLatencyFunction i r =
    RWObject r ->
    (AnnotatedInstruction i, Access) ->
    (AnnotatedInstruction i, Access) ->
    Maybe Latency
-- | Function that gives information about the use and definition
-- operands of an instruction
type OperandInfoFunction i rc = i -> ([OperandInfo rc], [OperandInfo rc])
-- | Function that gives aligned pairs of different instructions in an operation
type AlignedPairsFunction i r =
  BlockOperation i r -> [(Operand r, Operand r, Instruction i)]
-- | Function that gives packed pairs in an operation
type PackedPairsFunction i r = BlockOperation i r -> [(Operand r, Operand r)]
-- | Function that gives related pairs in an operation
type RelatedPairsFunction i r = BlockOperation i r -> [RegisterTable r]
-- | Copy instructions after the definition and before each use of a
-- temporary
type CopyInstructions i = ([Instruction i], [[Instruction i]])
-- | Target-dependent options (typically passed by tools through the
-- command line flag @--targetoption@)
type TargetOptions = [String]
-- | Target description together with target-dependent options
type TargetWithOptions i r rc s = (TargetDescription i r rc s, TargetOptions)

registerArray (ti, to) = tRegisterArray ti to
registerAtoms (ti, to) = tRegisterAtoms ti to
regClasses (ti, to) = tRegClasses ti to
registers (ti, to) = tRegisters ti to
infRegClassUsage (ti, to) rc @ InfiniteRegisterClass {} =
    tInfRegClassUsage ti to rc
infRegClassUsage _ _ =
    error ("infRegClassUsage is defined for infinite register classes only")
infRegClassBound (ti, to) rc @ InfiniteRegisterClass {} =
    tInfRegClassBound ti to rc
infRegClassBound _ _ =
    error ("infRegClassBound is defined for infinite register classes only")
subRegIndexType (ti, to) = tSubRegIndexType ti to
callerSaved (ti, to) = tCallerSaved ti to
calleeSaved (ti, to) = tCalleeSaved ti to
reserved (ti, to) = tReserved ti to
instructionType (ti, to) = tInstructionType ti to
branchInfo (ti, to) bo @ SingleOperation {oOpr = Natural i}
  | isBranch bo = Just (tBranchInfo ti to i)
branchInfo _ _ = Nothing
preProcess (ti, to) = tPreProcess ti to
postProcess (ti, to) = tPostProcess ti to
transforms (ti, to) = tTransforms ti to
copies (ti, to) = tCopies ti to
rematInstrs (ti, to) = tRematInstrs ti to
fromCopy (ti, to) ro o =
    o {oOpr = Natural (tFromCopy ti to (fmap (oNatural . oOpr) ro) (oOpr o))}
operandInfo (ti, to) = tOperandInfo ti to
alignedPairs (ti, to) o @ SingleOperation {oOpr = (Natural {})} =
  concat [[(p, q, tai) | (p, q) <- tAlignedPairs ti to i (oUses o, oDefs o)]
         | tai @ (TargetInstruction i) <- oInstructions o]
alignedPairs _ _ = []
packedPairs (ti, to) o =
  nub $ concat [tPackedPairs ti to i (oUses o, oDefs o) |
                TargetInstruction i <- oInstructions o]
relatedPairs (ti, to) SingleOperation {oOpr = Natural i} = tRelatedPairs ti to i
relatedPairs _ _ = []
resources (ti, to) = tResources ti to
usages (ti, to) op = tUsages ti to op
nop (ti, to) = Natural (tNop ti to)
readWriteInfo _ (General NullInstruction) = ([], [])
readWriteInfo (ti, to) (TargetInstruction op) = tReadWriteInfo ti to op
implementFrame (ti, to) = tImplementFrame ti to
addPrologue (ti, to) = tAddPrologue ti to
addEpilogue (ti, to) = tAddEpilogue ti to
stackDirection (ti, to) = tStackDirection ti to
readWriteLatency _ _ ((General NullInstruction, _), _) _ = Nothing
readWriteLatency _ _ _ ((General NullInstruction, _), _) = Nothing
readWriteLatency (ti, to) rwo (pi, pa) (ci, ca) =
    Just $ tReadWriteLatency ti to rwo (pi, pa) (ci, ca)
alternativeTemps (ti, to) = tAlternativeTemps ti to
expandCopy (ti, to) = tExpandCopy ti to
constraints (ti, to) = tConstraints ti to

-- | Container with information about a 'Function' along with the
-- function itself.

type FunctionInfo i r rc =
    (Function i r, S.Set (Operand r), CGraph i r, RegisterArray r rc,
     BCFGraph i r, Partition (Operand r))

-- | Description of a target processor. Implementing these functions
-- is all that is required for a Unison target. The following table specifies
-- which processor description functions are invoked by each component in the
-- Unison toolchain:
--
-- @
--  function            import linearize extend augment model export
--
--  'tRegisterArray'       x      -         x      -       x     x
--  'tRegisterAtoms'       x      -         x      -       x     x
--  'tRegClasses'          x      -         x      -       x     x
--  'tRegisters'           x      -         x      -       x     x
--  'tInfRegClassUsage'    x      -         x      -       x     x
--  'tInfRegClassBound'    x      -         x      -       x     x
--  'tSubRegIndexType'     x      -         -      -       -     -
--  'tCallerSaved'         x      -         -      -       x     -
--  'tCalleeSaved'         x      -         x      -       x     -
--  'tReserved'            x      -         -      -       -     -
--  'tInstructionType'     x      -         -      -       -     x
--  'tBranchInfo'          x      x         x      x       x     x
--  'tPreProcess'          x      -         -      -       -     -
--  'tPostProcess'         -      -         -      -       -     x
--  'tTransforms'          x      -         -      x       -     x
--  'tCopies'              -      -         x      -       -     -
--  'tRematInstrs'         -      -         x      -       -     -
--  'tFromCopy'            -      -         -      -       -     x
--  'tOperandInfo'         x      -         -      -       x     x
--  'tAlignedPairs'        -      -         x      -       x     -
--  'tPackedPairs'         -      -         -      -       x     -
--  'tRelatedPairs'        -      -         -      -       x     -
--  'tResources'           -      -         -      -       x     -
--  'tUsages'              -      -         -      -       x     x
--  'tNop'                 x      -         -      -       -     x
--  'tReadWriteInfo'       -      -         -      x       -     -
--  'tImplementFrame'      x      -         -      -       -     -
--  'tAddPrologue'         -      -         -      x       -     -
--  'tAddEpilogue'         -      -         -      x       -     -
--  'tStackDirection'      -      -         -      -       -     x
--  'tReadWriteLatency'    -      -         -      -       x     x
--  'tAlternativeTemps'    -      -         -      x       -     -
--  'tExpandCopy'          -      -         -      x       -     -
--  'tConstraints'         -      -         -      -       x     -
-- @

data TargetDescription i r rc s = TargetDescription {
      -- | Sequence of atomic (one register atom per register)
      -- register classes that forms the register array
      tRegisterArray    :: TargetOptions -> [RegisterClass rc],
      -- | First and last register atoms of the given register
      tRegisterAtoms    :: TargetOptions -> r -> (r, r),
      -- | Register classes
      tRegClasses       :: TargetOptions -> [RegisterClass rc],
      -- | Registers of each register class
      tRegisters        :: TargetOptions -> RegisterClass rc -> [r],
      -- | Number of atoms of each register in the given infinite
      -- register class
      tInfRegClassUsage :: TargetOptions -> RegisterClass rc -> Integer,
      -- | Possibly an upper bound of the number of register atoms in the given
      -- infinite register class
      tInfRegClassBound :: TargetOptions -> RegisterClass rc -> Maybe Integer,
      -- | Chained index type of the given sub-register index (see
      -- 'MachineSubRegIndex') for the given register class (in String
      -- format). A chain [t1, t2, .., tn] is to be interpreted as "the t1 part
      -- of the t2 part of .. of the tnth part of the register"
      tSubRegIndexType  :: TargetOptions -> String -> SubRegIndex -> [SubRegIndexType],
      -- | Caller-saved registers
      tCallerSaved      :: TargetOptions -> [r],
      -- | Callee-saved registers
      tCalleeSaved      :: TargetOptions -> [r],
      -- | Reserved, read-only registers
      tReserved         :: TargetOptions -> [r],
      -- | Type of the given instruction
      tInstructionType  :: TargetOptions -> i -> InstructionT,
      -- | Information about the branch performed by a 'Branch' operation
      tBranchInfo       :: TargetOptions -> NaturalOperation i r ->
                           BranchInfo,
      -- | Target-dependent 'MachineIR' transformations to be applied
      -- in the import phase
      tPreProcess       :: TargetOptions ->
                           [MachineFunction i r -> MachineFunction i r],
      -- | Target-dependent 'MachineIR' transformations to be applied
      -- in the export phase
      tPostProcess      :: TargetOptions ->
                           [MachineFunction i r -> MachineFunction i r],
      -- | Target-dependent transformations to be applied during the phase given
      -- by 'TransformPhase'
      tTransforms       :: TargetOptions -> TransformPhase ->
                           [FunctionTransform i r],
      -- | Copy instructions to extend the given temporary between the
      -- given definer and users
      tCopies           :: TargetOptions -> FunctionInfo i r rc -> Bool ->
                           Operand r -> [r] -> BlockOperation i r ->
                           [BlockOperation i r] -> CopyInstructions i,
      -- | Instructions to rematerialize the value defined by the
      -- given instruction (source instruction, dematerialization copy, and
      -- rematerialization copy)
      tRematInstrs      :: TargetOptions -> i -> Maybe (i, i, i),
      -- | Implementation of the given operation (typically a copy) to be
      -- applied during the export phase with possibly a supporting remat origin
      tFromCopy         :: TargetOptions -> Maybe (NaturalOperation i r) ->
                           Operation i r -> NaturalOperation i r,
      -- | Information about the use and definition operands of the given
      -- instruction
      tOperandInfo      :: TargetOptions -> OperandInfoFunction i rc,
      -- | Pairs of aligned operands (to be assigned the same register) for the
      -- given instruction and used and defined operands
      tAlignedPairs     :: TargetOptions -> i -> ([Operand r], [Operand r]) ->
                           [(Operand r, Operand r)],
      -- | Pairs of packed operands for the given instruction and used and
      -- defined operands
      tPackedPairs      :: TargetOptions -> i -> ([Operand r], [Operand r]) ->
                           [(Operand r, Operand r)],
      -- | Pairs of operands whose registers are related extensionally
      tRelatedPairs     :: TargetOptions -> NaturalOperation i r ->
                           [RegisterTable r],
      -- | Processor resources
      tResources        :: TargetOptions -> [Resource s],
      -- | Usages of the processor resources by the given instruction
      tUsages           :: TargetOptions -> i -> [Usage s],
      -- | No-operation
      tNop              :: TargetOptions -> NaturalOperation i r,
      -- | Read and written 'RWObject' by the given instruction
      tReadWriteInfo    :: TargetOptions -> i -> ([RWObject r], [RWObject r]),
      -- | Implementation of 'Frame' operations
      tImplementFrame   :: TargetOptions -> BlockOperation i r ->
                           [BlockOperation i r],
      -- | Prologue implementation in the augment phase
      tAddPrologue      :: TargetOptions -> BlockTransform i r,
      -- | Epilogue implementation in the augment phase
      tAddEpilogue      :: TargetOptions -> BlockTransform i r,
      -- | Direction in which the stack grows
      tStackDirection   :: TargetOptions -> StackDirection,
      -- | Latency of a 'RWObject' dependency between the given source
      -- and destination instruction.
      tReadWriteLatency :: TargetOptions -> RWObject r ->
                           (AnnotatedInstruction i, Access) ->
                           (AnnotatedInstruction i, Access) -> Latency,
      -- | Alternative temporaries for the given operand
      tAlternativeTemps :: TargetOptions -> [BlockOperation i r] ->
                           BlockOperation i r -> Operand r ->
                           [(Operand r, Maybe (BlockOperation i r))] ->
                           [Operand r],
      -- | Expanded copies for the given copy operation
      tExpandCopy       :: TargetOptions ->
                           Block i r ->
                           (OperationId, MoperandId, TemporaryId) ->
                           BlockOperation i r -> [BlockOperation i r],
      -- | Custom processor constraints
      tConstraints      :: TargetOptions -> Function i r ->
                           [ConstraintExpr i rc]
}

-- | Any 'TargetDescription'. Used to support multiple targets without
-- need to re-compile.

data Any td =
    forall i r rc s . (Eq i, Ord i, Show i, Read i,
                       Eq r, Ord r, Show r, Read r,
                       Eq rc, Ord rc, Show rc, Read rc,
                       Ord s, Show s, Read s) =>
    Any (td i r rc s)
