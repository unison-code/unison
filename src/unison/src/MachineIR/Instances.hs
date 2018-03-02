{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Instance declarations for the MIR representation (see
<http://llvm.org/docs/MIRLangRef.html>).

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

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
import MachineIR.Constructors

instance (Show i, Show r) => ShowSimple (MachineFunction i r) where
  showSimple mf = show (mf {mfIR = ""})

instance (Show i, Show r) => Show (MachineFunction i r) where
  show mf = showMir mf

showMir MachineFunction {mfName = name, mfProperties = mps,
                         mfBlocks = mbs, mfIR = ir} =
  let v = case find isMachineFunctionPropertyVersion mps of
             (Just (MachineFunctionPropertyVersion v')) -> v'
             Nothing   -> LLVM5
  in showIR ir ++ showMachineFunction v (name, mps, mbs)

showIR ir =
    docBegin ++ " |" ++ newLine ++
    nest' 2 ir ++ newLine ++
    docEnd ++ newLine

nest' n s = concatMap (\l -> replicate n ' ' ++ l ++ newLine) (lines s)

docBegin = "---"
docEnd = "..."

showMachineFunction v (name, mps, mbs) =
    docBegin ++ newLine ++
    fill 17 "name:" ++ name ++ newLine ++
    (case find isMachineFunctionPropertyFixedFrame mps of
        (Just mff) ->
          fill 17 "fixedStack:" ++ newLine ++
          nest' 2 (concatMap (showMachineFrameObjectInfo v) $
                   mfPropertyFixedFrame mff)
        Nothing -> "") ++
    (case find isMachineFunctionPropertyFrame mps of
        (Just mf) ->
          fill 17 "stack:" ++ newLine ++
          nest' 2 (concatMap (showMachineFrameObjectInfo v) $
                   mfPropertyFrame mf)
        Nothing -> "") ++
    (case find isMachineFunctionPropertyJumpTable mps of
        (Just mjt) ->
          fill 17 "jumpTable:" ++ newLine ++
          nest' 2 (showMachineJumpTable mjt)
        Nothing -> "") ++
    (case find isMachineFunctionPropertyRemovedFreqs mps of
        (Just mrf) ->
          fill 17 "removedFreqs:" ++
          show (mfPropertyRemovedFreqs mrf) ++ newLine
        Nothing -> "") ++
    fill 18 "body:" ++ "|" ++ newLine ++
    nest' 2 (showMachineBasicBlocks v mbs) ++ newLine ++
    docEnd ++ newLine

showMachineFrameObjectInfo v
  MachineFrameObjectInfo {mfoiIndex = id, mfoiOffset = off, mfoiSize = size,
                          mfoiAlignment = ali, mfoiCSRegister = csreg} =
  "- { id: " ++ show id ++ ", offset: " ++ show off ++
  (case size of
     Just s -> (", size: " ++ show s)
     Nothing -> "") ++
  ", alignment: " ++ show ali ++
  (case csreg of
     Just r -> (", callee-saved-register: '" ++ showMIRReg v r ++ "'")
     Nothing -> "") ++
  " }" ++ newLine

showMachineJumpTable (MachineFunctionPropertyJumpTable kind entries) =
  fill 17 "kind:" ++ kind ++ newLine ++
  fill 17 "entries:" ++ newLine ++
  nest' 2 (concatMap showMachineJumpTableEntry entries)

showMachineJumpTableEntry (MachineJumpTableEntry id bs) =
  fill 19 "- id:" ++ show id ++ newLine ++
  fill 19 "  blocks:" ++ "[ " ++
  showCS (quoted (showMachineOperand LLVM5)) bs ++ " ]" ++ newLine

showMachineBasicBlocks _ [] = ""
showMachineBasicBlocks v [mb] =
    showMachineBasicBlock v mb
showMachineBasicBlocks v (mb : mbs) =
    showMachineBasicBlock v mb ++ newLine ++
    showMachineBasicBlocks v mbs

showMachineBasicBlock v
  MachineBlock {mbId = bid, mbProperties = mps, mbInstructions = mis} =
  let freq  = find isMachineBlockPropertyFreq mps
      succs = find isMachineBlockPropertySuccs mps
      split = find isMachineBlockPropertySplit mps
  in "bb." ++ show bid ++
         (case freq of
            (Just (MachineBlockPropertyFreq f)) -> " (freq " ++ show f ++ ")"
            Nothing -> "") ++ ":" ++ newLine ++
         (case succs of
             (Just (MachineBlockPropertySuccs s)) | not (null s) ->
               nest' 2 ("successors: " ++ (showCS showSuccessor s) ++ newLine)
             _ -> "") ++
         (case split of
             (Just {}) -> nest' 2 ("split" ++ newLine)
             Nothing -> "") ++ newLine ++
     nest' 2 (showMachineBasicBlockBody v mis)

showSuccessor (bid, p) = "%bb." ++ show bid ++ "(" ++ show p ++ ")"

showCS f l = renderStyle lineStyle (cs f l)

quoted f s = "'" ++ f s ++ "'"
doubleQuoted f s = "\"" ++ f s ++ "\""

showMachineBasicBlockBody v mis = concatMap (showMachineInstruction v) mis

instance (Show i, Show r) => Show (MachineInstruction i r) where
    show = (showMachineInstruction LLVM5)

showMachineInstruction v (MachineBundle {mbHead = True, mbInstrs = mis}) =
  "BUNDLE" ++ showMachineBundleTail v mis

showMachineInstruction v (MachineBundle {mbHead = False, mbInstrs = mi:mis}) =
  showInlineMachineSingle v mi ++ showMachineBundleTail v mis

showMachineInstruction v ms @ MachineSingle {} =
  showInlineMachineSingle v ms ++ newLine

showInlineMachineSingle v (MachineSingle mopc mps mops) =
  let  n        = case find isMachineInstructionPropertyDefs mps of
                    (Just (MachineInstructionPropertyDefs n')) -> fromInteger n'
                    Nothing -> 0
       (ds, us) = splitAt n mops
  in (if null ds then "" else showCS (showMachineOperand v) ds ++ " = ") ++
     show mopc ++
     (if null us then "" else " " ++ showCS (showMachineOperand v) us)

showMachineBundleTail v mis =
  " {" ++ newLine ++
  nest' 2 (concatMap (showMachineInstruction v) mis) ++
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

showMachineOperand :: Show r => MachineIRVersion -> MachineOperand r -> String
showMachineOperand v (MachineReg name states) =
  let regPrefix = case v of
                    LLVM5 -> "%"
                    LLVM6 -> "$"
  in showCS showMachineRegState states ++
     (if null states then "" else " ") ++ regPrefix ++ show name
showMachineOperand _ (MachineImm value) = show value
showMachineOperand _ (MachineBlockRef id) = "%bb." ++ show id
showMachineOperand _ (MachineGlobalAddress address offset) =
  "@" ++ maybeEscape address ++ maybeShowOffset offset
showMachineOperand _ (MachineSymbol name) = "<mcsymbol " ++ name ++ ">"
showMachineOperand _ (MachineJumpTableIndex index) =
  jumpTablePrefix ++ show index
showMachineOperand v (MachineExternal name) =
  let extPrefix = case v of
                    LLVM5 -> "$"
                    LLVM6 -> "&"
  in extPrefix ++ name
showMachineOperand _ (MachineTemp id states td) =
  showCS showMachineRegState states ++
  (if null states then "" else " ") ++ "%" ++ show id ++ showTiedDef td
showMachineOperand _ (MachineFrameIndex idx fixed offset) =
  "%" ++ (if fixed then "fixed-" else "") ++ "stack." ++ show idx ++
  (if offset == 0 then "" else ("+" ++ show offset))
showMachineOperand _ MachineFrameSize = "%frame-size"
showMachineOperand _ (MachineMemPartition address partition) =
  "<0x" ++ showHex address "" ++ "> = !{!\"unison-memory-partition\", i32 " ++
  show partition ++ "}"
showMachineOperand _ (MachineProperty address property) =
  "<0x" ++ showHex address "" ++ "> = !{!\"unison-property\", !\"" ++
  property ++ "\"}"
showMachineOperand _ (MachineBlockFreq address freq) =
  "<0x" ++ showHex address "" ++ "> = !{!\"unison-block-frequency\", i32 " ++
  show freq ++ "}"
showMachineOperand _ (MachineDebugLocation id) = "debug-location !" ++ show id
showMachineOperand _ MachineNullReg = "_"
showMachineOperand v (MachineCFIDef reg off) =
  ".cfi_def_cfa " ++ showMIRReg v reg ++ ", " ++ show off
showMachineOperand _ (MachineCFIDefOffset off) =
  ".cfi_def_cfa_offset " ++ show off
showMachineOperand v (MachineCFIDefReg reg) =
  ".cfi_def_cfa_register " ++ showMIRReg v reg ++ reg
showMachineOperand v (MachineCFIOffset reg off) =
  ".cfi_offset " ++ showMIRReg v reg ++ ", " ++ show off
showMachineOperand _ (MachineCFIAdjustCfaOffset off) =
  ".cfi_adjust_cfa_offset " ++ show off
showMachineOperand _ (MachineRegMask name) = "csr_" ++ name
showMachineOperand _ (MachineConstantPoolIndex idx) = "%const." ++ idx
showMachineOperand _ (MachineFPImm i f e) =
  "float " ++ show i ++ "." ++ show f ++ "e" ++ showOffset e
showMachineOperand _ (MachineRawFPImm imm) = "float " ++ "0x" ++ showHex imm ""
showMachineOperand _ mo = show mo

showMIRReg v r = showMachineOperand v (mkMachineReg r)

jumpTablePrefix = "%jump-table."

showTiedDef Nothing = ""
showTiedDef (Just id) = "(tied-def " ++ show id ++ ")"

showMachineRegState MachineRegImplicit = "implicit"
showMachineRegState MachineRegImplicitDefine = "implicit-def"
showMachineRegState MachineRegUndef = "undef"

maybeEscape ga @ (d:_)
  | isDigit d = doubleQuoted id ("\\" ++ ga)
maybeEscape ga = ga

maybeShowOffset 0 = ""
maybeShowOffset n = showOffset n

showOffset n
  | n < 0  = "-" ++ show n
  | n >= 0 = "+" ++ show n

instance Show r => Show (MachineOperand r) where
  show (MachineTemp id _ _) = inBraces ["temp", show id]
  show (MachineSubTemp id subreg) =
    inBraces ["subtemp", inBraces [show id, subreg]]
  show (MachineSubRegIndex subreg) = inBraces ["subreg", subreg]
  show (MachineReg name _) = inBraces ["reg", show name]
  show (MachineImm value) = inBraces ["imm", show value]
  show (MachineFPImm int fr exp) = inBraces ["fpi", show int, show fr, show exp]
  show (MachineRawFPImm imm) = inBraces ["rfpi", show imm]
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
  show (MachineCFIAdjustCfaOffset off) = inBraces ["mcaco", show off]

inBraces es = "{" ++ renderStyle lineStyle (cs id es) ++ "}"

instance Show r => Show (MachineFrameObjectInfo r) where
    show (MachineFrameObjectInfo idx off size align csreg) =
        concat (intersperse "\'"
                (fmap show [Just idx, Just off, size, Just align] ++
                 [show csreg]))
