{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Instance declarations for the MIR representation (see
<http://llvm.org/docs/MIRLangRef.html>).

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE FlexibleContexts #-}
module MachineIR.Instances (showMir, showMachineOperand, jumpTablePrefix) where

import Data.List
import Data.Char
import Text.PrettyPrint
import Numeric

import Common.Util

import MachineIR.Base
import MachineIR.Predicates

instance (Show i, Show r) => ShowSimple (MachineFunction i r) where
  showSimple mf = show (mf {mfIR = ""})

instance (Show i, Show r) => Show (MachineFunction i r) where
  show mf = showMir mf

showMir MachineFunction {mfName = name, mfProperties = mps,
                         mfBlocks = mbs, mfIR = ir} =
  showIR ir ++ showMachineFunction (name, mps, mbs)

showIR ir =
    docBegin ++ " |" ++ newLine ++
    nest' 2 ir ++ newLine ++
    docEnd ++ newLine

nest' n s = concatMap (\l -> replicate n ' ' ++ l ++ newLine) (lines s)

docBegin = "---"
docEnd = "..."

showMachineFunction (name, mps, mbs) =
    docBegin ++ newLine ++
    fill 17 "name:" ++ name ++ newLine ++
    (case find isMachineFunctionPropertyFixedFrame mps of
        (Just mff) ->
          fill 17 "fixedStack:" ++ newLine ++
          nest' 2 (concatMap showMachineFrameObjectInfo $
                   mfPropertyFixedFrame mff)
        Nothing -> "") ++
    (case find isMachineFunctionPropertyFrame mps of
        (Just mf) ->
          fill 17 "stack:" ++ newLine ++
          nest' 2 (concatMap showMachineFrameObjectInfo $ mfPropertyFrame mf)
        Nothing -> "") ++
    (case find isMachineFunctionPropertyJumpTable mps of
        (Just mjt) ->
          fill 17 "jumpTable:" ++ newLine ++
          nest' 2 (showMachineJumpTable mjt)
        Nothing -> "") ++
    fill 18 "body:" ++ "|" ++ newLine ++
    nest' 2 (showMachineBasicBlocks mbs) ++ newLine ++
    docEnd ++ newLine

showMachineFrameObjectInfo
  MachineFrameObjectInfo {mfoiIndex = id, mfoiOffset = off, mfoiSize = size,
                          mfoiAlignment = ali} =
  "- { id: " ++ show id ++ ", offset: " ++ show off ++
  (case size of
     Just s -> (", size: " ++ show s)
     Nothing -> "") ++
  ", alignment: " ++ show ali ++ " }" ++ newLine

showMachineJumpTable (MachineFunctionPropertyJumpTable kind entries) =
  fill 17 "kind:" ++ kind ++ newLine ++
  fill 17 "entries:" ++ newLine ++
  nest' 2 (concatMap showMachineJumpTableEntry entries)

showMachineJumpTableEntry (MachineJumpTableEntry id bs) =
  fill 19 "- id:" ++ show id ++ newLine ++
  fill 19 "  blocks:" ++ "[ " ++
  showCS (quoted showMachineOperand) bs ++ " ]" ++ newLine

showMachineBasicBlocks [] = ""
showMachineBasicBlocks [mb] =
    showMachineBasicBlock mb
showMachineBasicBlocks (mb : mbs) =
    showMachineBasicBlock mb ++ newLine ++
    showMachineBasicBlocks mbs

showMachineBasicBlock
  MachineBlock {mbId = bid, mbProperties = mps, mbInstructions = mis} =
  let freq  = find isMachineBlockPropertyFreq mps
      succs = find isMachineBlockPropertySuccs mps
  in "bb." ++ show bid ++
         (case freq of
            (Just (MachineBlockPropertyFreq f)) -> " (freq " ++ show f ++ ")"
            Nothing -> "") ++ ":" ++ newLine ++
         (case succs of
             (Just (MachineBlockPropertySuccs s)) | not (null s) ->
               nest' 2 ("successors: " ++ (showCS showSuccessor s) ++ newLine)
             _ -> "") ++ newLine ++
     nest' 2 (showMachineBasicBlockBody mis)

showSuccessor (bid, p) = "%bb." ++ show bid ++ "(" ++ show p ++ ")"

showCS f l = renderStyle lineStyle (cs f l)

quoted f s = "'" ++ f s ++ "'"
doubleQuoted f s = "\"" ++ f s ++ "\""

showMachineBasicBlockBody mis = concatMap showMachineInstruction mis

showMachineInstruction (MachineBundle {mbHead = True, mbInstrs = mis}) =
  "BUNDLE" ++ showMachineBundleTail mis

showMachineInstruction (MachineBundle {mbHead = False, mbInstrs = mi:mis}) =
  showInlineMachineSingle mi ++ showMachineBundleTail mis

showMachineInstruction ms @ MachineSingle {} =
  showInlineMachineSingle ms ++ newLine

showInlineMachineSingle (MachineSingle mopc mps mops) =
  let  n        = case find isMachineInstructionPropertyDefs mps of
                    (Just (MachineInstructionPropertyDefs n')) -> fromInteger n'
                    Nothing -> 0
       (ds, us) = splitAt n mops
  in (if null ds then "" else showCS showMachineOperand ds ++ " = ") ++
     show mopc ++
     (if null us then "" else " " ++ showCS showMachineOperand us)

showMachineBundleTail mis =
  " {" ++ newLine ++
  nest' 2 (concatMap showMachineInstruction mis) ++
  "}" ++ newLine

instance Show r => Show (MachineFunctionProperty r) where
  show (MachineFunctionPropertyTriple triple) =
    inBraces ["triple", triple]
  show (MachineFunctionPropertyFrame frame) =
    inBraces ["frameobjs", concat [show mfoi ++ "_" | mfoi <- frame]]
  show (MachineFunctionPropertyRegClasses classes) =
    inBraces ["regclasses", classes]
  show (MachineFunctionPropertyJumpTable kind _) =
    inBraces ["jumptables", show kind, "(TODO)"]

instance Show MachineBlockProperty where
  show (MachineBlockPropertyFreq freq) = inBraces ["freq", show freq]
  show (MachineBlockPropertySuccs succs) =
      inBraces ["succs", inBraces (map showBlockId succs)]

showBlockId id = "b" ++ show id

instance Show i => Show (MachineOpcode i) where
  show (MachineVirtualOpc opc) = show opc
  show (MachineTargetOpc i) = show i

instance Show r => Show (MachineInstructionProperty r) where
  show (MachineInstructionPropertyMem mem) = inBraces ["mem", show mem]
  show (MachineInstructionPropertyCustom text) = inBraces ["custom", text]
  show (MachineInstructionPropertyOpFlags flags) = inBraces ["opmeta", flags]
  show (MachineInstructionPropertyJTIBlocks bs) =
    inBraces ["jtiblocks", inBraces (map show bs)]
  show (MachineInstructionPropertyDefs ds) = inBraces ["defs", show ds]

showMachineOperand :: Show r => MachineOperand r -> String
showMachineOperand (MachineReg name states) =
  showCS showMachineRegState states ++
  (if null states then "" else " ") ++ "%" ++ show name
showMachineOperand (MachineImm value) = show value
showMachineOperand (MachineBlockRef id) = "%bb." ++ show id
showMachineOperand (MachineGlobalAddress address offset) =
  "@" ++ maybeEscape address ++ showOffset offset
showMachineOperand (MachineSymbol name) = "<mcsymbol " ++ name ++ ">"
showMachineOperand (MachineJumpTableIndex index) = jumpTablePrefix ++ show index
showMachineOperand (MachineExternal name) = "$" ++ name
showMachineOperand (MachineTemp id td) = "%" ++ show id ++ showTiedDef td
showMachineOperand (MachineFrameIndex idx fixed offset) =
  "%" ++ (if fixed then "fixed-" else "") ++ "stack." ++ show idx ++
  (if offset == 0 then "" else ("+" ++ show offset))
showMachineOperand MachineFrameSize = "%frame-size"
showMachineOperand (MachineMemPartition address partition) =
  "<0x" ++ showHex address "" ++ "> = !{!\"unison-memory-partition\", i32 " ++
  show partition ++ "}"
showMachineOperand (MachineProperty address property) =
  "<0x" ++ showHex address "" ++ "> = !{!\"unison-property\", !\"" ++
  property ++ "\"}"
showMachineOperand (MachineDebugLocation id) = "debug-location !" ++ show id
showMachineOperand MachineNullReg = "_"
showMachineOperand (MachineCFIDef reg off) = ".cfi_def_cfa %" ++ reg ++ ", " ++ show off
showMachineOperand (MachineCFIDefOffset off) = ".cfi_def_cfa_offset " ++ show off
showMachineOperand (MachineCFIDefReg reg) = ".cfi_def_cfa_register %" ++ reg
showMachineOperand (MachineCFIOffset reg off) = ".cfi_offset %" ++ reg ++ ", " ++ show off
showMachineOperand (MachineRegMask name) = "csr_" ++ name
showMachineOperand (MachineConstantPoolIndex idx) = "%const." ++ idx
showMachineOperand mo = show mo

jumpTablePrefix = "%jump-table."

showTiedDef Nothing = ""
showTiedDef (Just id) = "(tied-def " ++ show id ++ ")"

showMachineRegState MachineRegImplicit = "implicit"
showMachineRegState MachineRegImplicitDefine = "implicit-def"

maybeEscape ga @ (d:_)
  | isDigit d = doubleQuoted id ("\\" ++ ga)
maybeEscape ga = ga

showOffset 0 = ""
showOffset n
  | n < 0 = "-" ++ show n
  | n > 0 = "+" ++ show n

instance Show r => Show (MachineOperand r) where
  show (MachineTemp id _) = inBraces ["temp", show id]
  show (MachineSubTemp id subreg) =
    inBraces ["subtemp", inBraces [show id, subreg]]
  show (MachineSubRegIndex subreg) = inBraces ["subreg", subreg]
  show (MachineReg name _) = inBraces ["reg", show name]
  show (MachineImm value) = inBraces ["imm", show value]
  show (MachineFPImm int fr exp) = inBraces ["fpi", show int, show fr, show exp]
  show (MachineBlockRef id) = inBraces ["mbb", show id]
  show (MachineFrameIndex index fixed offset) =
    inBraces ["fi", show index, show fixed, show offset]
  show (MachineFrameObject off size align) =
    inBraces ["mfo", show off, show size, show align]
  show MachineFrameSize = inBraces ["mfs"]
  show (MachineExternal name) = inBraces ["ext", name]
  show (MachineGlobalAddress address offset) =
    inBraces ["ga", inBraces ["@" ++ address, show offset]]
  show (MachineSymbol name) = inBraces ["sym", name]
  show (MachineJumpTableIndex index) = inBraces ["jti", show index]
  show (MachineRegMask list) = inBraces ["rmask", list]
  show (MachineConstantPoolIndex index) = inBraces ["cpi", index]
  show (MachineCFIIndex index) = inBraces ["cfi", show index]
  show (MachineMemPartition address id) = inBraces ["mmp", show address, show id]
  show MachineNullReg = inBraces ["reg", "-"]
  show (MachineDebugLocation index) = inBraces ["mdl", show index]
  show (MachineCFIDef name off) = inBraces ["mcd", name, show off]
  show (MachineCFIDefOffset off) = inBraces ["mcdo", show off]
  show (MachineCFIDefReg name) = inBraces ["mcdr", name]
  show (MachineCFIOffset name off) = inBraces ["mco", name, show off]

inBraces es = "{" ++ renderStyle lineStyle (cs id es) ++ "}"

instance Show MachineFrameObjectInfo where
    show (MachineFrameObjectInfo idx off size align) =
        concat (intersperse "\'"
                (fmap show [Just idx, Just off, size, Just align]))
