{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Basic definitions for LLVM's machine IR program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}

{-# LANGUAGE DeriveDataTypeable #-}

module MachineIR.Base
       (
         MachineFunction (..),
         MachineFunctionProperty (..),
         MachineJumpTableEntry (..),
         MachineBlock (..),
         MachineBlockProperty (..),
         MachineInstruction (..),
         MachineOpcode (..),
         MachineVirtualOpcode (..),
         MachineInstructionProperty (..),
         MachineOperand (..),
         MachineRegState (..),
         MachineFrameObjectInfo (..),
         MachineIRVersion (..)
       )
       where

import Data.Data

-- | Machine IR function corresponding to LLVM's @MachineFunction@. This
-- data type and all the data types contained ('MachineBlock',
-- 'MachineInstruction', ...) are parameterized over 'i' and 'r', which are
-- instruction and register types supplied by the 'TargetDescription'.

data MachineFunction i r = MachineFunction {
  -- | Function name
  mfName       :: String,
  -- | Function-level properties
  mfProperties :: [MachineFunctionProperty r],
  -- | Blocks in the function (the first one is the entry block)
  mfBlocks     :: [MachineBlock i r],
  -- | Original LLVM IR
  mfIR         :: String
} deriving (Eq, Ord)

-- | Machine function-level properties.

data MachineFunctionProperty r =
  -- | Target triple
  MachineFunctionPropertyTriple {
    mfPropertyTriple :: String
    } |
  -- | Stack frame corresponding to LLVM's @MachineFrameInfo@ (fixed part)
  MachineFunctionPropertyFixedFrame {
    mfPropertyFixedFrame :: [MachineFrameObjectInfo r]
    } |
  -- | Stack frame corresponding to LLVM's @MachineFrameInfo@ (variable part)
  MachineFunctionPropertyFrame {
    mfPropertyFrame :: [MachineFrameObjectInfo r]
    } |
  -- | Register class of each temporary embedded as a string
  MachineFunctionPropertyRegClasses {
    mfPropertyRegClasses :: String
    } |
  -- | Jump table information
  MachineFunctionPropertyJumpTable {
    -- | Kind of jump table corresponding to LLVM's @JTEntryKind@
    mfPropertyJumpTableKind :: String,
    -- | Jump table entries
    mfPropertyJumpTable :: [MachineJumpTableEntry r]
    } |
  -- | Frequencies of removed blocks
  MachineFunctionPropertyRemovedFreqs {
    mfPropertyRemovedFreqs :: [Integer]
    } |
  -- | MIR version for parsing and emission
  MachineFunctionPropertyVersion {
    mfPropertyVersion :: MachineIRVersion
    }
  deriving (Eq, Ord)

-- | Machine jump table entry

data MachineJumpTableEntry r = MachineJumpTableEntry {
  -- | Jump table indentifier
  mjtId :: Integer,
  -- | Jump table blocks
  mjtBlocks :: [MachineOperand r]
} deriving (Eq, Ord)

-- | Machine IR block corresponding to LLVM's @MachineBasicBlock@.

data MachineBlock i r = MachineBlock {
  -- | Block identifier
  mbId           :: Integer,
  -- | Block-level properties
  mbProperties   :: [MachineBlockProperty],
  -- | Machine instructions contained in the block
  mbInstructions :: [MachineInstruction i r]
  } deriving (Eq, Ord)

-- | Successor (block id and probability)
type MachineSuccessor = (Integer, Integer)

-- | Machine block-level properties.

data MachineBlockProperty =
  -- | Block's execution frequency
  MachineBlockPropertyFreq {
    mbPropertyFreq :: Integer
    } |
  -- | Block's successors
  MachineBlockPropertySuccs {
    mbPropertySuccs :: [MachineSuccessor]
    } |
  -- | Whether the block has been split
  MachineBlockPropertySplit {}
  deriving (Eq, Ord)

-- | Machine IR instruction corresponding to LLVM's @MachineInstr@.

data MachineInstruction i r =
  -- | Machine IR bundle instruction containing single machine instructions
  -- (corresponds to a LLVM @MachineInstr@ where @isBundle() == true@)
  MachineBundle {
    -- | Whether the LLVM's pseudo-opcode 'BUNDLE' is the header (otherwise the
    -- first instruction in 'mbInstrs' is the bundle header)
    mbHead :: Bool,
    -- | Instructions in the bundle
    mbInstrs :: [MachineInstruction i r]
    } |
  -- | Machine IR primitive instruction
  MachineSingle {
    -- | Opcode of the machine IR instruction
    msOpcode     :: MachineOpcode i,
    -- | Instruction-level properties
    msProperties :: [MachineInstructionProperty r],
    -- | Definition and use operands (the definition operands appear first
    -- followed by the use operands)
    msOperands   :: [MachineOperand r]
    }
  deriving (Eq, Ord)

-- | Machine IR opcode.

data MachineOpcode i =
  -- | Virtual machine opcode
  MachineVirtualOpc {
    mopcVirtual :: MachineVirtualOpcode
    } |
  -- | Target machine opcode (corresponds to higher, target-dependent
  -- values in LLVM's @TargetOpcode@).
  MachineTargetOpc {
    mopcTarget  :: i
    }
  deriving (Eq, Ord)

-- | Machine IR virtual opcode (corresponds partially to lower values in
-- LLVM's @TargetOpcode@).

data MachineVirtualOpcode =
  PHI |
  COPY |
  ENTRY |
  RETURN |
  EXIT |
  EXTRACT_SUBREG |
  LOW |
  HIGH |
  SPLIT2 |
  SPLIT4 |
  IMPLICIT_DEF |
  INSERT_SUBREG |
  REG_SEQUENCE |
  SUBREG_TO_REG |
  COMBINE |
  ADJCALLSTACKUP |
  ADJCALLSTACKDOWN |
  ANNOTATION_LABEL |
  CFI_INSTRUCTION |
  EH_LABEL |
  BLOCK_MARKER |
  FREE_OPCODE String |
  BUNDLE
  deriving (Eq, Ord, Show, Read)

-- | Machine instruction-level properties.

data MachineInstructionProperty r =
  -- | Memory partition accessed by the instruction
  MachineInstructionPropertyMem {
    msPropertyMem :: Integer
    } |
  -- | Custom, target-dependent property
  MachineInstructionPropertyCustom {
    msPropertyCustom :: String
    } |
  -- | Operand flags (LLVM's @IsDef@, @IsImp@, @IsKill@, @IsDead@, ...) embedded
  -- as a string
  MachineInstructionPropertyOpFlags {
    msPropertyOpFlags :: String
    } |
  -- | Target blocks of a jump table
  MachineInstructionPropertyJTIBlocks {
    msPropertyJTIBlocks :: [MachineOperand r]
    } |
  -- | Number of definition operands (for pretty-printing purposes)
  MachineInstructionPropertyDefs {
    msPropertyDefs :: Integer
    } |
  -- | Whether the branch is predicted to be taken (only applies to conditional
  -- branches)
  MachineInstructionPropertyBranchTaken {
    msPropertyBranchTaken :: Bool
    }
  deriving (Eq, Ord)

-- | Machine IR operand corresponding to LLVM's @MachineOperand@.

data MachineOperand r =
  -- | Temporary corresponding to LLVM's @MO_Register@ when this is a virtual register
  MachineTemp {
    mtId      :: Integer,
    mtFlags   :: [MachineRegState],
    mtTiedDef :: Maybe Integer
    } |
  -- | Temporary corresponding to LLVM's @MO_Register@ when this is a virtual
  -- register with a sub-register (in LLVM, @getSubReg() != 0@)
  MachineSubTemp {
    mstId          :: Integer,
    mstSubRegIndex :: String
    } |
  -- | Sub-register index corresponding to LLVM's @MO_Immediate@ when used by a
  -- @INSERT_SUBREG@, @REG_SEQUENCE@, or @SUBREG_TO_REG@ instruction
  MachineSubRegIndex {
    msrSubRegIndex :: String
    } |
  -- | Register corresponding to LLVM's @MO_Register@
  MachineReg {
    mrName    :: r,
    mrFlags   :: [MachineRegState]
    } |
  -- | Immediate corresponding to LLVM's @MO_Immediate@
  MachineImm {
    miValue :: Integer
    } |
  -- | Floating-point immediate corresponding to LLVM's @MO_FPImmediate@
  MachineFPImm {
    mfpInteger    :: Integer,
    mfpFractional :: Integer,
    mfpExponent   :: Integer
    } |
  -- | "Raw" floating-point immediate in hexadecimal corresponding to
  -- LLVM's @MO_FPImmediate@ in later versions of LLVM
  MachineRawFPImm {
    mrfpValue :: Integer
    } |
  -- | Block reference corresponding to LLVM's @MO_MachineBasicBlock@
  MachineBlockRef {
    mbrId :: Integer
    } |
  -- | Frame index corresponding to LLVM's @MO_FrameIndex@
  MachineFrameIndex {
    mfiIndex  :: Integer,
    mfiFixed  :: Bool,
    mfiOffset :: Integer
    } |
  -- | Frame object (internal use, does not correspond to any LLVM type)
  MachineFrameObject {
    mfoOffset    :: Integer,
    mfoSize      :: Maybe Integer,
    mfoAlignment :: Integer
    } |
  -- | Frame size (internal use, does not correspond to any LLVM type)
  MachineFrameSize |
  -- | External symbol corresponding to LLVM's @MO_ExternalSymbol@
  MachineExternal {
    meName :: String
    } |
  -- | Address of a global value corresponding to LLVM's @MO_GlobalAddress@
  MachineGlobalAddress {
    mgaAddress :: String,
    mgaOffset  :: Integer
    } |
  -- | LLVM 'MCSymbol' reference corresponding to LLVM's @MO_MCSymbol@
  MachineSymbol {
    msName :: String
    } |
  -- | Jump table address corresponding to LLVM's @MO_JumpTableIndex@
  MachineJumpTableIndex {
    mjtiIndex :: Integer
    } |
  -- | Bit mask of preserved registers corresponding to LLVM's @MO_RegisterMask@
  MachineRegMask {
    mrmName :: String
    } |
  -- | Constant pool index corresponding to LLVM's @MO_ConstantPoolIndex@
  MachineConstantPoolIndex {
    mcpiIndex :: String
    } |
  -- | CFI index corresponding to LLVM's @MO_CFIIndex@
  MachineCFIIndex {
    mcfiIndex :: Integer
    } |
  -- | Memory partition (does not correspond to any LLVM operand, we represent
  -- it in LLVM with a metadata operand)
  MachineMemPartition {
    mmpAddress :: Integer,
    mmpId      :: Integer
    } |
  -- | Custom Unison property (does not correspond to any LLVM operand, we
  -- represent it in LLVM with a metadata operand)
  MachineProperty {
    mpAddress  :: Integer,
    mpProperty :: String
    } |
  -- | Block frequency (does not correspond to any LLVM operand, we represent
  -- it in LLVM with a metadata operand)
  MachineBlockFreq {
    mbfAddress :: Integer,
    mbfFreq    :: Integer
    } |
  -- | Null register corresponding to LLVM's @MO_Register@ called @-@
  MachineNullReg |
  -- | Debug location (does not correspond to any LLVM operand)
  MachineDebugLocation {
    mdlId :: Integer
    } |
  -- | CFI definition register with offset (for LLVM's @CFI_INSTRUCTION@ instructions)
  MachineCFIDef {
    mcdRegName :: String,
    mcdOffset :: Integer
    } |
  -- | CFI definition offset (for LLVM's @CFI_INSTRUCTION@ instructions)
  MachineCFIDefOffset {
    mcdOffset :: Integer
    } |
  -- | CFI definition register (for LLVM's @CFI_INSTRUCTION@ instructions)
  MachineCFIDefReg {
    mcdRegName :: String
    } |
  -- | CFI definition register with offset (for LLVM's @CFI_INSTRUCTION@ instructions)
  MachineCFIOffset {
    mcRegName :: String,
    mcOffset :: Integer
    } |
  -- | Free-form register (does not correspond to any LLVM operand, for
  -- temporary use only)
  MachineFreeReg {
    mfrRegName :: String
    }
  deriving (Eq, Ord)

-- | State of a machine register corresponding to LLVM's @RegState@

data MachineRegState =
  -- | Implicit operand
  MachineRegImplicit |
  -- | Implicit definition
  MachineRegImplicitDefine |
  -- | Undefined value
  MachineRegUndef
  deriving (Eq, Ord)

-- | Object allocated in the stack corresponding to LLVM's 'StackObject'.

data MachineFrameObjectInfo r =
  MachineFrameObjectInfo {
    -- | Object index
    mfoiIndex      :: Integer,
    -- | Object offset
    mfoiOffset     :: Integer,
    -- | Object size ('Nothing' if variable size)
    mfoiSize       :: Maybe Integer,
    -- | Object alignment
    mfoiAlignment  :: Integer,
    -- | Object callee-saved register ('Nothing' if not a callee-saved spill)
    mfoiCSRegister :: Maybe r
    } deriving (Eq, Ord)

-- | MIR version to be parsed.

data MachineIRVersion =
  -- | LLVM 5.0 or older
  LLVM5 |
  -- | LLVM 6.0 or newer (defines embedded register classes in the operands)
  LLVM6
  deriving (Data, Typeable, Show, Read, Eq, Ord)
