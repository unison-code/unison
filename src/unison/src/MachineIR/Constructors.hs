{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Functions to construct the machine IR program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

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
         -- * MachineJumpTableEntry constructors
         mkMachineJumpTableEntry,
         -- * MachineBlock constructors
         mkMachineBlock,
         -- * MachineBlockProperty constructors
         mkMachineBlockPropertyFreq,
         mkMachineBlockPropertySuccs,
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
         -- * MachineOperand constructors
         mkMachineTemp,
         mkMachineSubTemp,
         mkMachineSubRegIndex,
         mkMachineReg,
         mkMachineImm,
         mkMachineFPImm,
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
         mkMachineRegClass,
         mkMachineMemPartition,
         mkMachineProperty,
         mkMachineNullReg,
         mkMachineDebugLocation,
         mkMachineCFIDef,
         mkMachineCFIDefOffset,
         mkMachineCFIDefReg,
         mkMachineCFIOffset,
         -- * MachineRegState constructors
         mkMachineRegImplicit,
         mkMachineRegImplicitDefine,
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

mkMachineJumpTableEntry = MachineJumpTableEntry

mkMachineBlock = MachineBlock

mkMachineBlockPropertyFreq = MachineBlockPropertyFreq
mkMachineBlockPropertySuccs = MachineBlockPropertySuccs

mkMachineBundle = MachineBundle
mkMachineSingle = MachineSingle
mkBlockMarker id =
    mkMachineSingle (mkMachineVirtualOpc BLOCK_MARKER) [] [mkMachineBlockRef id]

mkMachineInstructionPropertyMem = MachineInstructionPropertyMem
mkMachineInstructionPropertyCustom = MachineInstructionPropertyCustom
mkMachineInstructionPropertyOpFlags = MachineInstructionPropertyOpFlags
mkMachineInstructionPropertyJTIBlocks = MachineInstructionPropertyJTIBlocks
mkMachineInstructionPropertyDefs = MachineInstructionPropertyDefs

mkMachineTemp = MachineTemp
mkMachineSubTemp = MachineSubTemp
mkMachineSubRegIndex = MachineSubRegIndex
mkMachineReg name = MachineReg name []
mkMachineImm = MachineImm
mkMachineFPImm = MachineFPImm
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
mkMachineRegClass = MachineRegClass
mkMachineMemPartition = MachineMemPartition
mkMachineProperty = MachineProperty
mkMachineNullReg = MachineNullReg
mkMachineDebugLocation = MachineDebugLocation
mkMachineCFIDef = MachineCFIDef
mkMachineCFIDefOffset = MachineCFIDefOffset
mkMachineCFIDefReg = MachineCFIDefReg
mkMachineCFIOffset = MachineCFIOffset

mkMachineRegImplicit = MachineRegImplicit
mkMachineRegImplicitDefine = MachineRegImplicitDefine

mkMachineTargetOpc = MachineTargetOpc
mkMachineVirtualOpc = MachineVirtualOpc
mkMachineFrameObjectInfo = MachineFrameObjectInfo
