{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Basic definitions for the Unison program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE DeriveDataTypeable #-}
module Unison.Base
       (
        -- * Generic types
        OperationId,
        BlockId,
        TemporaryId,
        MoperandId,
        ResourceId,
        RegisterAtomId,
        RegisterClassId,
        RegisterSpaceId,
        RegisterSpaceName,
        InstructionId,
        Latency,
        IssueCycle,
        Frequency,
        -- * Program types
        Function (..),
        Block (..),
        BlockOperation (..),
        Operation (..),
        Instruction (..),
        GeneralInstruction (..),
        RegisterId (..),
        NaturalOperation (..),
        VirtualOperation (..),
        DelimiterOperation (..),
        FrameOperation (..),
        Operand (..),
        CongruenceTuple,
        FrameObject (..),
        JumpTableEntry (..),
        HighLevelGoal (..),
        BlockAttributes (..),
        Attributes (..),
        RWObject (..),
        -- * Operation meta-types
        OperationT (..),
        NaturalT (..),
        VirtualT (..),
        DelimiterT (..),
        FrameT (..),
        -- * Instruction meta-types
        InstructionT (..),
        -- * Target types
        RegisterClass (..),
        OperandInfo (..),
        Access (..),
        SubRegIndex (..),
        SubRegIndexType (..),
        IndexedRegisterClass (..),
        RegisterSpace (..),
        RegisterAtom (..),
        BranchInfo (..),
        ConditionType (..),
        Resource (..),
        Usage (..),
        IndexedResource (..),
        IndexedUsage (..),
        RegisterTable (..),
        -- * Graph types
        BCFGraph,
        ICFGraph,
        DGraph,
        CGraph,
        OGraph,
        PGraph,
        ICFGLabel,
        CGEdgeLabel (..),
        OGEdgeLabel (..),
        DGEdgeLabel,
        -- * Constraint types
        ConstraintExpr (..),
        -- * Other types
        Dependency (..),
        PrecedenceType (..),
        OperationTransform,
        BlockTransform,
        FunctionTransform,
        TransformPhase (..),
        IndexedInstruction (..),
        AnnotatedInstruction,
        GoalDescription (..),
        Goal (..),
        GoalObject (..),
        Partitionable (..),
        Partition (..)
       )
       where

import Data.Typeable
import Data.Graph.Inductive
import MachineIR.Base

-- | 'Operation' identifier (e.g. @o15@).
type OperationId       = Integer
-- | 'Block' identifier (e.g. @b4@).
type BlockId           = Integer
-- | 'Temporary' identifier (e.g. @t2@).
type TemporaryId       = Integer
-- | 'Operand' identifier (e.g. @p37@).
type MoperandId        = Integer
-- | 'Resource' identifier.
type ResourceId        = Integer
-- | 'RegisterAtom' identifier.
type RegisterAtomId    = Integer
-- | 'RegisterClass' identifier.
type RegisterClassId   = Integer
-- | ''RegisterSpace' identifier.
type RegisterSpaceId   = Integer
-- | 'RegisterSpace' name.
type RegisterSpaceName = String
-- | 'ModelInstruction' identifier.
type InstructionId     = Integer
-- | Latency of a precedence.
type Latency           = Integer
-- | Operation issue cycle
type IssueCycle        = Integer
-- | Execution frequency.
type Frequency         = Integer

-- | Unison function. This data type and all the data types contained
-- ('Block', 'BlockOperation', ...) are parameterized over 'i' and 'r',
-- which are instruction and register types supplied by the
-- 'TargetDescription'.

data Function i r = Function {
      -- | Comments related to the function, pragmas, etc.
      fComments    :: [String],
      -- | Function name
      fName        :: String,
      -- | Blocks in the function (the first one is the entry block)
      fCode        :: [Block i r],
      -- | Congruences among operands
      fCongruences :: [CongruenceTuple r],
      -- | Operands that can be rematerialized
      fRematerializable :: [RematerializableTuple r],
      -- | Fixed stack frame information
      fFixedStackFrame :: [FrameObject r],
      -- | Variable stack frame information
      fStackFrame  :: [FrameObject r],
      -- | Stack pointer offset (default is 0)
      fStackPointerOffset :: Integer,
      -- | Argument-passing section size in the stack frame (default is 0)
      fStackArgSize :: Integer,
      -- | Constant information (id, value, alignment)
      fConstants :: [(Integer, String, Integer)],
      -- | Jump table kind and entries
      fJumpTable   :: (String, [JumpTableEntry]),
      -- | Goal(s) for which the function is to be optimized
      fGoal        :: [HighLevelGoal],
      -- | Removed frequencies
      fRemovedFreqs :: [Frequency],
      -- | Source program (e.g. LLVM IR)
      fSource      :: String
    } deriving Eq

-- | Unison block.

data Block i r = Block {
      -- | Block identifier
      bLab  :: BlockId,
      -- | Block attributes
      bAs   :: BlockAttributes,
      -- | Operations contained in the block
      bCode :: [BlockOperation i r]
    } deriving Eq

-- | Unison block operation. This data type acts as a container for an
-- 'Operation' within a block.

data BlockOperation i r =
    -- | Bundle operation containing single operations
    Bundle {
      bundleOs :: [BlockOperation i r]
    } |
    -- | Single (primitive) operation
    SingleOperation {
      -- | Single operation identifier
      oId  :: OperationId,
      -- | Contained operation
      oOpr :: Operation i r,
      -- | Single operation attributes
      oAs  :: Attributes i r
    } deriving Eq

-- | Unison operation.

data Operation i r =
    -- | Regular program operation
    Natural {
      oNatural :: NaturalOperation i r
    } |
    -- | Operation with special semantics that is not meant to appear in the
    -- final code
    Virtual (VirtualOperation r) |
    -- | Operation with copy semantics
    Copy {
      -- | Instructions that can implement the copy
      oCopyIs :: [Instruction i],
      -- | Source operand
      oCopyS   :: Operand r,
      -- | Rest of uses
      oCopyUs  :: [Operand r],
      -- | Destination operand
      oCopyD   :: Operand r,
      -- | Rest of definitions
      oCopyDs  :: [Operand r]
    } deriving (Eq, Ord)

-- | Regular program operation.

data NaturalOperation i r =
    -- | Operation that does not alter the control flow
    Linear {
      -- | Instructions that can implement the operation
      oIs :: [Instruction i],
      -- | Uses
      oUs :: [Operand r],
      -- | Definitions
      oDs :: [Operand r]
    } |
    -- | Operation that causes an internal branch
    Branch {
      -- | Instructions that can implement the operation
      oBranchIs :: [Instruction i],
      -- | Uses
      oBranchUs :: [Operand r]
    } |
    -- | Operation that implements a function call
    Call {
      -- | Instructions that can implement the operation
      oCallIs :: [Instruction i],
      -- | Uses
      oCallUs :: [Operand r]
    } |
    -- | Operation that implements a tail call
    TailCall {
      -- | Instructions that can implement the operation
      oTailCallIs :: [Instruction i],
      -- | Uses
      oTailCallUs :: [Operand r]
    }
    deriving (Eq, Ord)

-- | Operation with special semantics that is not meant to appear in the final
-- code.

data VirtualOperation r =
    -- | SSA phi-operation
    Phi {
      -- | Uses
      oPhiUs :: [Operand r],
      -- | Definition
      oPhiD  :: Operand r
    } |
    -- | Operation that marks the boundaries of parts of the function
    Delimiter (DelimiterOperation r) |
    -- | Operation that forces the use of some temporaries
    Kill {
      -- | Kill instructions
      oKillIs :: [GeneralInstruction],
      -- | Uses
      oKillUs :: [Operand r]
    } |
    -- | Operation that forces the definition of some temporaries
    Define {
      -- | Definitions
      oDefineDs :: [Operand r]
    } |
    -- | Operation that combines two temporaries assigned to contiguous register
    -- atoms into a single double-width temporary
    Combine {
      -- | Low use
      oCombineLowU  :: Operand r,
      -- | High use
      oCombineHighU :: Operand r,
      -- | Combined definition
      oCombineD     :: Operand r
    } |
    -- | Operation that defines a temporary to be assigned at the low register
    -- atom(s) of its used temporary
    Low {
      -- | Low instructions
      oLowIs :: [GeneralInstruction],
      -- | Double-width use
      oLowU :: Operand r,
      -- | Low definition
      oLowD :: Operand r
    } |
    -- | Operation that defines a temporary to be assigned at the high register
    -- atom(s) of its used temporary
    High {
      -- | High instructions
      oHighIs :: [GeneralInstruction],
      -- | Double-width use
      oHighU :: Operand r,
      -- | High definition
      oHighD :: Operand r
    } |
    -- | Operation that defines two temporaries, to be assigned at the low and
    -- high register atom(s) of its used temporary (can be seen as a combined
    -- (high) and (low) operation)
    Split2 {
      -- | Double-width use
      oSplit2U :: Operand r,
      -- | Low definition
      oSplit2LowD :: Operand r,
      -- | High definition
      oSplit2HighD :: Operand r
    } |
    -- | Operation that defines for temporaries, to be assigned at different
    -- atom(s) of its used temporary (can be seen as two combined (split2)
    -- operations)
    Split4 {
      -- | Four-width use
      oSplit4U :: Operand r,
      -- | Low-low definition
      oSplit4LowLowD :: Operand r,
      -- | Low-high definition
      oSplit4LowHighD :: Operand r,
      -- | High-low definition
      oSplit4HighLowD :: Operand r,
      -- | High-high definition
      oSplit4HighHighD :: Operand r
    } |
    -- | Operation representing an abstract copy from a source to a destination
    -- temporary which has not yet been assigned alternative copy instructions
    -- from the target processor
    VirtualCopy {
      -- | Source copy
      oVirtualCopyS :: Operand r,
      -- | Destination copy
      oVirtualCopyD :: Operand r
    } |
    -- | Operation representing the uses and definitions that follow a function
    -- call
    Fun {
      -- | Uses
      oFunctionUs :: [Operand r],
      -- | Definitions
      oFunctionDs :: [Operand r]
    } |
    -- | Operation that marks the boundaries of operations that handle the stack
    -- frame
    Frame (FrameOperation r)
    deriving (Eq, Ord)

-- | Operation that marks the boundaries of parts of the function.

data DelimiterOperation r =
    -- | Operation that marks the beginning of a 'Block'
    In {
      -- | Definitions (block live-ins)
      oIns :: [Operand r]
    } |
    -- | Operation that marks the end of a 'Block'
    Out {
      -- | Uses (block live-outs)
      oOuts :: [Operand r]
    } |
    -- | Operation that marks the beginning of a 'Function'
    Entry {
      -- | Definitions (function live-ins)
      oEntry :: [Operand r]
    } |
    -- | Operation that marks the return from a 'Function'
    Return {
      -- | Uses (function live-outs)
      oReturn :: [Operand r]
    } |
    -- | Operation that marks the exit from a 'Function'
    Exit
    deriving (Eq, Ord)

-- | Operation that marks the boundaries of operations that handle the stack
-- frame.

data FrameOperation r =
    -- | Operation that marks the beginning of a frame setup prior to a function
    -- call
    Setup {
      -- | Use (typically a stack offset)
      oSetupU :: Operand r
    } |
    -- | Operation that marks the end of a frame destruction after a function
    -- call
    Destroy {
      -- | Use (typically a stack offset)
      oDestroyU :: Operand r
    }
    deriving (Eq, Ord)

-- | Instruction that can implement an operation.

data Instruction i =
  -- | Instruction of any target
  General {
    oGeneralInstr :: GeneralInstruction
  } |
  -- | Instruction of the target processor
  TargetInstruction {
    oTargetInstr :: i
  }
  deriving (Eq, Ord)

-- | Instruction that does not belong to any particular target.

data GeneralInstruction =
  -- | Instruction to indicate that an operation is inactive
  NullInstruction |
  -- | Instruction that does not use any processor resources
  VirtualInstruction |
  -- | Instruction that creates a resource barrier
  BarrierInstruction
  deriving (Eq, Ord)

-- | Register identifier

data RegisterId r =
    -- | Register of the target processor
    TargetRegister {
      -- | Actual register
      rTargetReg :: r
    } |
    -- | Register that belongs to an infinite register class
    InfiniteRegister {
      -- | Register name prefix
      rInfRegPre   :: String,
      -- | Register atom range
      rInfRegRange :: (RegisterAtom, RegisterAtom)
    }
    deriving (Eq, Ord)

-- | Attributes of a Unison 'Block'.

data BlockAttributes = BlockAttributes {
  -- | Whether the block is a function entry
  aEntry  :: Bool,
  -- | Whether the block is a function exit
  aExit   :: Bool,
  -- | Whether the block is a function return
  aReturn :: Bool,
  -- | Possibly the block's execution frequency
  aFreq   :: Maybe Frequency,
  -- | Whether the block has been split
  aSplit  :: Bool
} deriving (Eq)

-- | Attributes of a Unison 'BlockOperation'.

data Attributes i r = Attributes {
  -- | Read objects
  aReads        :: [RWObject r],
  -- | Written objects
  aWrites       :: [RWObject r],
  -- | Identifier of the related call operation, if any
  aCall         :: Maybe OperationId,
  -- | Identifier of the memory section accessed by reads and writes, if any
  aMem          :: Maybe Integer,
  -- | Instructions that activate the operation
  aActivators   :: [Instruction i],
  -- | Whether the operation derives from a virtual copy
  aVirtualCopy  :: Bool,
  -- | Whether the operation rematerializes a value
  aRemat        :: Bool,
  -- | Jump table target blocks, if any
  aJTBlocks     :: [BlockId],
  -- | Whether the branch is predicted to be taken (only applies to conditional
  -- branches)
  aBranchTaken  :: Maybe Bool,
  -- | Whether the operation is prescheduled and in what issue cycle
  aPrescheduled :: Maybe IssueCycle,
  -- | Identifier of the rematerialization copy's origin, if any
  aRematOrigin  :: Maybe OperationId
} deriving (Eq)

-- | Object representing the side-effect of an 'Operation'. Operations can write
-- and read these objects to enforce serializations.

data RWObject r =
    -- | Memory section object
    Memory String |
    -- | Entire memory object
    AllMemory |
    -- | Control object to enforce control dependencies
    ControlSideEffect |
    -- | Program counter object
    ProgramCounterSideEffect |
    -- | Other type of side effect (reflected in the register file)
    OtherSideEffect r
    deriving (Eq, Ord)

-- | Unison operand.

data Operand r =
    -- | Storage location holding a value (typically corresponding to a
    -- high-level program variable)
    Temporary {
      -- | Temporary identifier
      tId  :: TemporaryId,
      -- | Possibly a register to which the temporary is pre-assigned
      tReg :: Maybe (Operand r)
      } |
    -- | Null connection (&#x22A5;) for a 'MOperand'
    NullTemporary |
    -- | Operand that can be connected to alternative temporaries as introduced
    -- in
    -- <https://www.sics.se/%7ercas/publications/CastanedaCarlssonEa_LCTES_2014.pdf the LCTES2014 paper>
    MOperand {
      -- | Operand identifier
      operandId  :: MoperandId,
      -- | Alternative temporaries that can be connected to the operand
      altTemps   :: [Operand r],
      -- | Possibly a register to which the operand is pre-assigned
      operandReg :: Maybe (Operand r)
    } |
    -- | Reference to a block
    BlockRef {
      -- | Block identifier
      blockRefId :: BlockId
    } |
    -- | Processor register
    Register {
      -- | Register identifier
      regId :: RegisterId r
    } |
    -- | Operand that is unchanged by Unison
    Bound (MachineOperand r) |
    -- | Reference to another operand
    OperandRef {
      operandRefId :: MoperandId
    }

instance Eq r => Eq (Operand r) where
  (Temporary t _) == (Temporary t' _) = (t == t')
  NullTemporary == NullTemporary = True
  (MOperand i a r) == (MOperand i' a' r') =
    (i, a, r) == (i', a', r')
  (BlockRef i) == (BlockRef i') = (i == i')
  (Register r) == (Register r') = (r == r')
  (Bound e) == (Bound e') = (e == e')
  (OperandRef p) == (OperandRef p') = (p == p')
  _ == _ = False

instance Ord r => Ord (Operand r) where
  compare (Temporary t _) (Temporary t' _) = compare t t'
  compare NullTemporary NullTemporary = EQ
  compare (MOperand i a r) (MOperand i' a' r') =
      compare (i, a, r) (i', a', r')
  compare (BlockRef i) (BlockRef i') = compare i i'
  compare (Register r) (Register r') = compare r r'
  compare (Bound e) (Bound e') = compare e e'
  compare (OperandRef p) (OperandRef p') = compare p p'
  compare o1 o2 | o1 /= o2 =
    let score op =
          case op of
            Temporary {} -> 0
            NullTemporary -> 1
            MOperand {} -> 2
            BlockRef {} -> 3
            Register {} -> 4
            Bound {} -> 5
            OperandRef {} -> 6
    in compare (score o1) (score o2)

instance (Show r) => Show (RegisterId r) where
  show (TargetRegister r) = show r
  show (InfiniteRegister pre (fa, la)) =
      pre ++ show (raId fa) ++
      if fa == la then "" else "-" ++ show (raId la)

instance Show RegisterAtom where
    show (RegisterAtom a) = "A" ++ show a

instance (Read r) => Read (RegisterId r) where
  -- Always interpret as a target register
  readsPrec _ name = [(TargetRegister (read name), "")]

type CongruenceTuple r = (Operand r, Operand r)
type RematerializableTuple r = (Operand r, [OperationId])

-- | Object allocated in the stack frame similar to LLVM's 'StackObject'.

data FrameObject r =
  FrameObject {
    -- | Object index: there are two different indices depending on whether the
    -- object is fixed (belongs to 'fFixedStackFrame') or variable (belongs to
    -- 'fStackFrame')
    foIndex       :: Integer,
    -- | Object offset
    foOffset      :: Integer,
    -- | Object size ('Nothing' if variable size)
    foSize        :: Maybe Integer,
    -- | Object alignment
    foAlignment   :: Integer,
    -- | Object callee-saved register ('Nothing' if not a callee-saved spill)
    foCSRegister  :: Maybe r
    } deriving (Eq, Ord)

-- | Jump table entry

data JumpTableEntry = JumpTableEntry {
  -- | Jump table indentifier
  jtId :: Integer,
  -- | Jump table block ids
  jtBlocks :: [BlockId]
} deriving (Eq, Ord)

-- | High-level optimization goal.
data HighLevelGoal =
  -- | Speed optimization (corresponds to 'DynamicGoal Cycles')
  Speed |
  -- | Code size optimization (corresponds to 'StaticGoal BundleWidth')
  Size |
  -- | Spill code overhead optimization (corresponds to 'DynamicGoal Spill')
  Spill
  deriving (Eq, Ord)

-- | Types corresponding to 'Operation'.

data OperationT =
    NaturalType NaturalT |
    VirtualType VirtualT |
    CopyType
    deriving (Eq, Ord, Show)

-- | Types corresponding to 'NaturalOperation'.

data NaturalT =
    LinearType |
    BranchType |
    CallType |
    TailCallType
    deriving (Eq, Ord, Show)

-- | Types corresponding to 'VirtualOperation'.

data VirtualT =
    PhiType |
    DelimiterType DelimiterT |
    KillType |
    DefineType |
    CombineType |
    LowType |
    HighType |
    Split2Type |
    Split4Type |
    VirtualCopyType |
    FunType |
    FrameType FrameT
    deriving (Eq, Ord, Show)

-- | Types corresponding to 'DelimiterOperation'.

data DelimiterT =
    InType |
    OutType |
    EntryType |
    ReturnType |
    ExitType
    deriving (Eq, Ord, Show)

-- | Types corresponding to 'FrameOperation'.

data FrameT =
    SetupType |
    DestroyType
    deriving (Eq, Ord, Show)

-- | Types corresponding to processor instructions. Used to infer the type of an
-- 'Operation' from its alternative instructions.

data InstructionT =
    LinearInstructionType |
    CopyInstructionType |
    BranchInstructionType |
    CallInstructionType |
    TailCallInstructionType
    deriving (Eq, Ord, Show)

-- | Collection of processor registers. This data type is parameterized
-- over 'rc', a register class type supplied by the 'TargetDescription'.

data RegisterClass rc =
    -- | Regular register class
    RegisterClass rc |
    -- | Register class with a practically infinite supply of register atoms
    -- (typically, the stack)
    InfiniteRegisterClass rc |
    -- | Abstract register class to be instantiated
    AbstractRegisterClass rc |
    -- | Register class containing all atomic registers
    TopRegisterClass
    deriving (Eq, Show, Ord)

-- | Register class with an identifier. This data type acts as a container for a
-- 'RegisterClass'.

data IndexedRegisterClass rc = IndexedRegisterClass {
  -- | Register class identifier
  rcId    :: RegisterClassId,
  -- | Register class
  rcClass :: RegisterClass rc
}

-- | Register atom range. Each register space corresponds to one or more of
-- 'RegisterClass'.

data RegisterSpace = RegisterSpace {
  -- | Register space identifier
  rsId       :: RegisterSpaceId,
  -- | Register space name
  rsName     :: RegisterSpaceName,
  -- | Whether the register space corresponds to an 'InfiniteRegisterClass'
  rsInfinite :: Bool
}

-- | Minimum part of a physical register in the register array that can be
-- referenced by an instruction.

data RegisterAtom = RegisterAtom {
  -- | Register atom identifier
  raId :: RegisterAtomId
} deriving (Eq, Ord, Typeable)

-- | Processor resource such as a functional unit or a bus.

data Resource s = Resource {
  -- | Resource
  resName     :: s,
  -- | Resource capacity
  resCapacity :: Integer
} deriving Show

-- | Usage of a processor resource by a particular processor instruction.

data Usage s = Usage {
  -- | Resource name
  resource   :: s,
  -- | Units of the processor resource used
  units      :: Integer,
  -- | Duration of the usage of processor resource
  occupation :: Integer,
  -- | Offset of the usage of processor resource
  offset     :: Integer
} deriving (Eq, Ord, Show)

-- | Processor resource with an identifier. This data type acts as a container
-- for a 'Resource'.

data IndexedResource s = IndexedResource {
  -- | Resource identifier
  resId :: ResourceId,
  -- | Processor resource
  res   :: Resource s
} deriving Show

-- | Usage of a processor resource with an identifier. This data type acts as a
-- container for a 'Usage'.

data IndexedUsage s = IndexedUsage {
  -- | Resource identifier
  resourceId :: ResourceId,
  -- | Usage
  usage      :: Usage s
} deriving (Eq, Ord, Show)

-- | Model instruction with an identifier. This data type acts as a container
-- for a 'ModelInstruction'.

data IndexedInstruction i = IndexedInstruction {
  -- | Instruction identifier
  ioId        :: InstructionId,
  -- | Model instruction
  ioInstruction :: Instruction i
} deriving (Eq, Ord)

-- | Instruction annotated with the type of the operation that contains it.

type AnnotatedInstruction i = (Instruction i, OperationT)

-- | Information about an operand of a processor instruction.

data OperandInfo rc =
    -- | Specifies that the corresponding operand is a 'Temporary' or 'MOperand'
    -- and gives some additional information
    TemporaryInfo {
      -- | Register class of the operand
      oiRegClass  :: RegisterClass rc,
      -- | Latency of the operand
      oiLatency   :: Latency,
      -- | Whether the operand is bypassing
      oiBypassing :: Bool
    } |
    -- | Specifies that the corresponding operand is a 'Bound'
    BoundInfo |
    -- | Specifies that the corresponding operand is a 'BlockRef'
    BlockRefInfo
    deriving (Eq, Ord, Show)

-- | Type of access to a 'RWObject'.

data Access =
    -- | Object read
    Read |
    -- | Object write
    Write
    deriving (Eq, Ord, Show)

-- | LLVM sub-register index.

data SubRegIndex =
   -- | Named sub-register index
   NamedSubRegIndex String |
   -- | Raw sub-register index
   RawSubRegIndex Integer
   deriving (Eq, Show)

-- | Type of LLVM sub-register index.

data SubRegIndexType =
    -- | Specifies that the sub-register index corresponds to the low part
    LowSubRegIndex |
    -- | Specifies that the sub-register index corresponds to the high part
    HighSubRegIndex |
    -- | Specifies that the sub-register index corresponds to both parts
    CopySubRegIndex
    deriving (Eq, Show)

-- | Information about the branch performed by a 'Branch' operation.

data BranchInfo = BranchInfo {
  -- | Type of branch
  branchCond   :: ConditionType,
  -- | Destination of the branch, if known
  branchTarget :: Maybe BlockId
} deriving (Eq)

-- | Type of condition for a 'Branch' operation.

data ConditionType =
    -- | Conditional branch
    Conditional |
    -- | Unconditional branch
    Unconditional
    deriving (Eq, Show)

-- | Relation among the registers of a pair of operands

data RegisterTable r = RegisterTable {
  -- | First involved operand
  rrFirst  :: Operand r,
  -- | Second involved operand
  rrSecond :: Operand r,
  -- | Relation tuples
  rrTuples :: [(r, r)]
  } deriving (Eq)

-- | 'Block' control-flow graph.
type BCFGraph i r = Gr (Block i r) ()
-- | 'BlockOperation' control-flow graph.
type ICFGraph i r = Gr (ICFGLabel i r) ()
-- | Dependency graph among 'BlockOperation'.
type DGraph i r = Gr (BlockOperation i r) (DGEdgeLabel r)
-- | Congruence graph.
type CGraph i r = Gr (Bool, Maybe (Operand r)) (CGEdgeLabel i r)
-- | 'Operand' graph
type OGraph i r = Gr (Operand r) (OGEdgeLabel i r)
-- | Precedence graph among 'BlockOperation'.
type PGraph i r = Gr (BlockOperation i r) PrecedenceType
-- | Label of a 'ICFGraph'.
type ICFGLabel i r = (BlockId, BlockOperation i r)
-- | Label of a 'DGraph'.
type DGEdgeLabel r = (Dependency r, Bool, [Maybe Latency])
-- | Label of a 'CGraph'.
data CGEdgeLabel i r =
    -- | Congruence among operands
    CongruenceEdge Bool |
    -- | Copy among operands
    CopyEdge (BlockOperation i r) |
    -- | Combination of operands
    CombineEdge (BlockOperation i r) |
    -- | Low part of an operand
    LowEdge (BlockOperation i r) |
    -- | High part of an operand
    HighEdge (BlockOperation i r) |
    -- | Arbitrary part of an operand
    SplitEdge (BlockOperation i r)
-- | Label of a 'OGraph'.
data OGEdgeLabel i r =
    -- | Flow of data across operands
    DataFlowEdge (Operand r) |
    -- | Copy among operands
    OperandCopyEdge (BlockOperation i r) |
    -- | Operands that belong to same 'Operation'
    OperandNaturalEdge (BlockOperation i r) |
    -- | Congruent operands
    OperandCongruenceEdge

-- | Expression over a Unison IR function.

data ConstraintExpr i rc =
    -- | Disjunction
    OrExpr [ConstraintExpr i rc] |
    -- | Conjunction
    AndExpr [ConstraintExpr i rc] |
    -- | Exclusive or
    XorExpr (ConstraintExpr i rc) (ConstraintExpr i rc) |
    -- | Implication
    ImpliesExpr (ConstraintExpr i rc) (ConstraintExpr i rc) |
    -- | Negation
    NotExpr (ConstraintExpr i rc) |
    -- | Literal expressing that an operation is active
    ActiveExpr OperationId |
    -- | Literal expressing that an operand is connected to a
    -- certain temporary
    ConnectsExpr MoperandId TemporaryId |
    -- | Literal expressing that an operation is implemented by an
    -- instruction
    ImplementsExpr OperationId (Instruction i) |
    -- | Literal expressing that two instructions are issued at least n
    -- cycles from each other
    DistanceExpr OperationId OperationId Latency |
    -- | Literal expressing that two operands are connected to the same
    -- temporary
    ShareExpr MoperandId MoperandId |
    -- | Literal expressing that the live ranges of the temporaries connected to
    -- two operands overlap
    OperandOverlapExpr MoperandId MoperandId |
    -- | Literal expressing that the live ranges of two temporaries overlap
    TemporaryOverlapExpr TemporaryId TemporaryId |
    -- | Literal expressing that a temporary is assigned to a caller-saved
    -- register
    CallerSavedExpr TemporaryId |
    -- | Literal expressing that the temporary connected to an operand is
    -- assigned to a register class
    AllocatedExpr MoperandId (RegisterClass rc) |
    -- | Emission-only, internal version of operation implementation literal
    EImplementsExpr OperationId (IndexedInstruction i) |
    -- | Emission-only, internal version of temporary allocation literal
    EAllocatedExpr MoperandId (IndexedRegisterClass rc)

-- | Dependency among 'Operation's.

data Dependency r =
    -- | Dependency on a temporary that is defined and used
    DataDependency [Operand r] |
    -- | Dependency caused by two accesses to a side-effect object
    ReadWriteDependency (RWObject r) (Access, Access) |
    -- | Control dependency
    ControlDependency |
    -- | Dependency that is derived from other dependencies
    ExtendedDependency |
    -- | Dependency that is introduced for secondary purposes (e.g. for
    -- pretty-printing)
    FakeDependency deriving (Eq, Ord)

-- | Type of precendence among 'Operation's.

data PrecedenceType =
    -- | Precedence with a positive latency
    PositivePrecedence |
    -- | Precedence with a non-negative latency
    Precedence |
    -- | Precedence with a negative latency
    NegativePrecedence deriving (Eq, Ord, Show)

-- | Type of a function that transforms some of a block of 'BlockOperation'
-- given some context.

type OperationTransform i r =
    Function i r
    -- ^ Entire function (context)
    -> [BlockOperation i r]
    -- ^ Input block
    -> (TemporaryId, OperationId, MoperandId)
    -- ^ Identifiers not yet used by any temporary, operation, or operand in the
    -- current function
    -> ([BlockOperation i r], [BlockOperation i r])
    -- ^ Remaining of the block left to process (first) and transformed
    -- operations (second)

-- | Type of a function that transforms an entire block of 'BlockOperation'
-- given some context.

type BlockTransform i r =
    (TemporaryId, OperationId, MoperandId)
    -- ^ Identifiers not yet used in the current function
    -> [BlockOperation i r]
    -- ^ Input block
    -> [BlockOperation i r]
    -- ^ Transformed block

-- | Type of a function that transforms an entire 'Function'.

type FunctionTransform i r =
    Function i r
    -- ^ Input function
    -> Function i r
    -- ^ Transformed function

-- | Phase in which target-specific transformations can be invoked.

data TransformPhase =
    -- | During import phase, before register lifting
    ImportPreLift |
    -- | During import phase, after register lifting
    ImportPostLift |
    -- | During import phase, after enforcing the calling convention
    ImportPostCC |
    -- | During augment phase, before adding read/write objects
    AugmentPreRW |
    -- | During augment phase, after adding read/write objects
    AugmentPostRW |
    -- | During export phase, before computing frame offsets
    ExportPreOffs |
    -- | During export phase, after computing frame offsets
    ExportPostOffs |
    -- | During export phase, before lowering frame objects
    ExportPreLow
    deriving Show

-- | Description of an objective goal to optimize for.

data GoalDescription s = GoalDescription String (Goal s) deriving Show

-- | Objective goal.

data Goal s =
    -- | Goal where block frequencies are not taken into account (e.g. code
    -- size)
    StaticGoal (GoalObject s) |
    -- | Goal where block frequencies are taken into account (e.g. speed)
    DynamicGoal (GoalObject s)
  deriving Show

-- | Object optimized by the goal.

data GoalObject s =
    -- | Total estimated execution cycles
    Cycles |
    -- | Usage of a certain resource
    ResourceUsage s
    deriving Show

-- | Something that can be partitioned into equivalence classes.
class Partitionable a where
    -- | Turns into a graph node
    toNode   :: a -> Int

-- | Partition.
data Partition a = Partition {
      -- | Graph that represents the partition
      graph    :: Gr () ()
    }
