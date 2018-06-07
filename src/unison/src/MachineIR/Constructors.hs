{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Functions to construct the machine IR program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module MachineIR.Constructors
       (
         -- * MachineFunction constructors
         mkMachineFunction,
         -- * MachineFunctionProperty constructors
         mkMachineFunctionPropertyTriple,
         mkMachineFunctionPropertyFixedFrame,
         mkMachineFunctionPropertyFrame,
         mkMachineFunctionPropertyRegClasses,
         mkMachineFunctionPropertyJumpTable,
         mkMachineFunctionPropertyRemovedFreqs,
         mkMachineFunctionPropertyVersion,
         mkMachineFunctionPropertyRegisters,
         mkMachineFunctionPropertyConstants,
         -- * MachineJumpTableEntry constructors
         mkMachineJumpTableEntry,
         -- * MachineBlock constructors
         mkMachineBlock,
         -- * MachineBlockProperty constructors
         mkMachineBlockPropertyFreq,
         mkMachineBlockPropertySuccs,
         mkMachineBlockPropertySplit,
         -- * MachineInstruction constructors
         mkMachineBundle,
         mkMachineSingle,
         mkBlockMarker,
         -- * MachineInstructionProperty constructors
         mkMachineInstructionPropertyMem,
         mkMachineInstructionPropertyCustom,
         mkMachineInstructionPropertyOpFlags,
         mkMachineInstructionPropertyJTIBlocks,
         mkMachineInstructionPropertyDefs,
         mkMachineInstructionPropertyBranchTaken,
         -- * MachineOperand constructors
         mkMachineTemp,
         mkSimpleMachineTemp,
         mkMachineSubTemp,
         mkMachineSubRegIndex,
         mkMachineReg,
         mkMachineCompleteReg,
         mkMachineImm,
         mkMachineFPImm,
         mkMachineRawFPImm,
         mkMachineBlockRef,
         mkMachineFrameIndex,
         mkMachineFrameObject,
         mkMachineFrameSize,
         mkMachineExternal,
         mkMachineGlobalAddress,
         mkMachineSymbol,
         mkMachineJumpTableIndex,
         mkMachineRegMask,
         mkMachineConstantPoolIndex,
         mkMachineCFIIndex,
         mkMachineMemPartition,
         mkMachineProperty,
         mkMachineBlockFreq,
         mkMachineNullReg,
         mkMachineDebugLocation,
         mkMachineCFIDef,
         mkMachineCFIDefOffset,
         mkMachineCFIDefReg,
         mkMachineCFIOffset,
         mkMachineCFIAdjustCfaOffset,
         mkMachineFreeReg,
         -- * MachineRegState constructors
         mkMachineRegImplicit,
         mkMachineRegImplicitDefine,
         mkMachineRegUndef,
         -- * Other
         mkMachineVirtualOpc,
         mkMachineTargetOpc,
         mkMachineFrameObjectInfo
       )
       where

import MachineIR.Base

mkMachineFunction = MachineFunction

mkMachineFunctionPropertyTriple = MachineFunctionPropertyTriple
mkMachineFunctionPropertyFixedFrame = MachineFunctionPropertyFixedFrame
mkMachineFunctionPropertyFrame = MachineFunctionPropertyFrame
mkMachineFunctionPropertyRegClasses = MachineFunctionPropertyRegClasses
mkMachineFunctionPropertyJumpTable = MachineFunctionPropertyJumpTable
mkMachineFunctionPropertyRemovedFreqs = MachineFunctionPropertyRemovedFreqs
mkMachineFunctionPropertyVersion = MachineFunctionPropertyVersion
mkMachineFunctionPropertyRegisters = MachineFunctionPropertyRegisters
mkMachineFunctionPropertyConstants = MachineFunctionPropertyConstants

mkMachineJumpTableEntry = MachineJumpTableEntry

mkMachineBlock = MachineBlock

mkMachineBlockPropertyFreq = MachineBlockPropertyFreq
mkMachineBlockPropertySuccs = MachineBlockPropertySuccs
mkMachineBlockPropertySplit = MachineBlockPropertySplit

mkMachineBundle = MachineBundle True
mkMachineSingle = MachineSingle
mkBlockMarker id =
    mkMachineSingle (mkMachineVirtualOpc BLOCK_MARKER) [] [mkMachineBlockRef id]

mkMachineInstructionPropertyMem = MachineInstructionPropertyMem
mkMachineInstructionPropertyCustom = MachineInstructionPropertyCustom
mkMachineInstructionPropertyOpFlags = MachineInstructionPropertyOpFlags
mkMachineInstructionPropertyJTIBlocks = MachineInstructionPropertyJTIBlocks
mkMachineInstructionPropertyDefs = MachineInstructionPropertyDefs
mkMachineInstructionPropertyBranchTaken = MachineInstructionPropertyBranchTaken

mkMachineTemp = MachineTemp
mkSimpleMachineTemp id = mkMachineTemp id [] Nothing
mkMachineSubTemp = MachineSubTemp
mkMachineSubRegIndex = MachineSubRegIndex
mkMachineReg name = MachineReg name []
mkMachineCompleteReg name states = MachineReg name states
mkMachineImm = MachineImm
mkMachineFPImm = MachineFPImm
mkMachineRawFPImm = MachineRawFPImm
mkMachineBlockRef = MachineBlockRef
mkMachineFrameIndex = MachineFrameIndex
mkMachineFrameObject = MachineFrameObject
mkMachineFrameSize = MachineFrameSize
mkMachineExternal = MachineExternal
mkMachineGlobalAddress = MachineGlobalAddress
mkMachineSymbol = MachineSymbol
mkMachineJumpTableIndex = MachineJumpTableIndex
mkMachineRegMask = MachineRegMask
mkMachineConstantPoolIndex = MachineConstantPoolIndex
mkMachineCFIIndex = MachineCFIIndex
mkMachineMemPartition = MachineMemPartition
mkMachineProperty = MachineProperty
mkMachineBlockFreq = MachineBlockFreq
mkMachineNullReg = MachineNullReg
mkMachineDebugLocation = MachineDebugLocation
mkMachineCFIDef = MachineCFIDef
mkMachineCFIDefOffset = MachineCFIDefOffset
mkMachineCFIDefReg = MachineCFIDefReg
mkMachineCFIOffset = MachineCFIOffset
mkMachineCFIAdjustCfaOffset = MachineCFIAdjustCfaOffset
mkMachineFreeReg = MachineFreeReg

mkMachineRegImplicit = MachineRegImplicit
mkMachineRegImplicitDefine = MachineRegImplicitDefine
mkMachineRegUndef = MachineRegUndef

mkMachineTargetOpc = MachineTargetOpc
mkMachineVirtualOpc = MachineVirtualOpc
mkMachineFrameObjectInfo = MachineFrameObjectInfo
