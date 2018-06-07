{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Instance declarations for the Unison program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Instances
       (
        showAccess,
        showCongruences,
        showInstrs,
        showAlternatives,
        equaling
       )
       where

import Data.Maybe
import Data.Ord
import Data.Function (on)
import Text.PrettyPrint

import Common.Util

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Constructors

import MachineIR.Base (MachineIRVersion(..))
import MachineIR.Instances

instance (Ord i, Eq r) => Ord (BlockOperation i r) where
    compare SingleOperation {oId = i} SingleOperation {oId = j} =
        compare i j
    compare (Bundle bi) (Bundle bj) = compare bi bj
    compare _ _ = LT

instance Enum (Operand r) where
    toEnum                   = mkTemp
    fromEnum (Temporary t _) = fromInteger t

instance Num (Operand r) where
    (Temporary a _) + (Temporary b _) = Temporary (a + b) Nothing
    (Temporary a _) * (Temporary b _) = Temporary (a * b) Nothing
    negate (Temporary a r)            = Temporary (-a) r
    abs (Temporary a r)               = Temporary (abs a) r
    signum (Temporary a r)            = Temporary (signum a) r
    fromInteger                       = mkTemp

instance Show r => Partitionable (Operand r) where
    toNode (Temporary t _)            = fromInteger t
    toNode (OperandRef p)             = fromInteger p
    toNode (MOperand {operandId = p}) = fromInteger p
    toNode o = error ("unmatched: toNode " ++ show o)

instance Partitionable Int where
    toNode = id

instance Partitionable Integer where
    toNode = fromInteger

instance Show r => Show (Dependency r) where
  show (DataDependency os)         = "Data (" ++ show os ++ ")"
  show (ReadWriteDependency rwo a) = "Read/Write (" ++ showAccess a ++ ", " ++
                                      show rwo ++ ")"
  show ControlDependency           = "Control"
  show ExtendedDependency          = "Extended"
  show FakeDependency              = "Fake"

showAccess (Read, Read) = "RAR"
showAccess (Read, Write) = "WAR"
showAccess (Write, Read) = "RAW"
showAccess (Write, Write) = "WAW"

instance Eq (IndexedRegisterClass rc) where (==) = equaling rcId
instance Ord (IndexedRegisterClass rc) where compare = comparing rcId

instance Eq RegisterSpace where (==) = equaling rsId
instance Ord RegisterSpace where compare = comparing rsId

instance Enum RegisterAtom where
    toEnum = RegisterAtom . toInteger
    fromEnum = fromInteger . raId
instance Num RegisterAtom where
    (RegisterAtom a) + (RegisterAtom b) = RegisterAtom (a + b)
    (RegisterAtom a) * (RegisterAtom b) = RegisterAtom (a * b)
    negate (RegisterAtom a)             = RegisterAtom (-a)
    abs (RegisterAtom a)                = RegisterAtom (abs a)
    signum (RegisterAtom a)             = RegisterAtom (signum a)
    fromInteger                         = RegisterAtom

instance (Eq i, Eq r) => Eq (CGEdgeLabel i r) where
    (CopyEdge bi) == CopyEdge (bi')           = bi == bi'
    (CongruenceEdge b) == (CongruenceEdge b') = b == b'
    (CombineEdge bi) == (CombineEdge bi')     = bi == bi'
    (LowEdge bi) == (LowEdge bi')             = bi == bi'
    (HighEdge bi) == (HighEdge bi')           = bi == bi'
    (SplitEdge bi) == (SplitEdge bi')         = bi == bi'
    _ == _                                    = False

instance (Eq i, Eq r) => Eq (OGEdgeLabel i r) where
    (DataFlowEdge t) == (DataFlowEdge t')               = t == t'
    (OperandCopyEdge bi) == (OperandCopyEdge bi')       = bi == bi'
    (OperandNaturalEdge bi) == (OperandNaturalEdge bi') = bi == bi'
    OperandCongruenceEdge == OperandCongruenceEdge      = True
    _ == _                                              = False

equaling = ((==) `on`)

instance (Show i, Show r) => ShowSimple (Function i r) where
  showSimple f = show (f {fSource = ""})

instance (Show i, Show r) => Show (Function i r) where
    show (Function comments name code cs' rts ffobjs fobjs sp ss consts jt goal
          rfs src) =
      concatMap showComment comments ++
      showSectionName "function" ++ " " ++ name ++ newLine ++
      concatMap show code ++
      showSectionName "adjacent" ++ newLine ++
      showCongruences cs' lineWidth wsWidth ++
      showSectionName "rematerializable" ++ newLine ++
      showRematerializable rts lineWidth wsWidth ++
      showSectionName "fixed-frame" ++ newLine ++
      showFrameObjects True ffobjs ++
      showSectionName "frame" ++ newLine ++
      showFrameObjects False fobjs ++
      showSectionName "stack-pointer-offset" ++ " " ++ show sp ++ newLine ++
      showSectionName "stack-arg-size" ++ " " ++ show ss ++ newLine ++
      showSectionName "constants" ++ newLine ++
      showConstantObjects consts ++
      showSectionName "jump-table" ++ newLine ++
      showJumpTableEntries jt ++
      showSectionName "goal" ++ maybeShowGoal goal ++ newLine ++
      showSectionName "removed-freqs" ++
      (if null rfs then "" else " " ++ render (cs show rfs)) ++ newLine ++
      showSectionName "source" ++ newLine ++
      src

showComment comment = "//" ++ comment ++ newLine

maybeShowGoal [] = ""
maybeShowGoal gs = " " ++ render (cs show gs)

instance Show HighLevelGoal where
  show Speed = "speed"
  show Size  = "size"
  show Spill = "spill"

instance (Show i, Show r) => Show (Block i r) where
  show (Block l as code) =
      let w = maxIdWidth code + 2
      in "b" ++ show l ++ show as ++ ":" ++ newLine ++
         concatMap (showLineInst w) code

instance (Show i, Show r) => Show (BlockOperation i r) where
    show = showBlockOperation 0

showBlockOperation w (SingleOperation id i as) =
    fill w ("o" ++ show id ++ ": ") ++ show i ++ show as

showBlockOperation _ (Bundle is) =
    "{" ++ renderStyle lineStyle (csWith ";" show is) ++ "}"

instance (Show i, Show r) => Show (Operation i r) where
  show o = showDs o ++ " <- " ++ showInstrs o ++ " " ++ showUs o

instance (Show i) => Show (Instruction i) where
  show (General NullInstruction)     = "-"
  show (General VirtualInstruction) = "virtual"
  show (General BarrierInstruction)  = "barrier"
  show (TargetInstruction i)         = show i

instance (Show i) => Show (IndexedInstruction i) where
  show (IndexedInstruction id i) = show (id, i)

instance (Show rc) => Show (IndexedRegisterClass rc) where
  show (IndexedRegisterClass id rc) = show (id, rc)

instance Show RegisterSpace where
  show (RegisterSpace id rs inf) = show (id, rs, inf)

instance (Show i) => Show (CGEdgeLabel i r) where
    show (CopyEdge bi)      = "copy (" ++ show (oId bi) ++ ")"
    show (CongruenceEdge b) = "congruence" ++ if b then " (boundary)" else ""
    show (CombineEdge bi)   = "combine (" ++ show (oId bi) ++ ")"
    show (LowEdge bi)       = "low (" ++ show (oId bi) ++ ")"
    show (HighEdge bi)      = "high (" ++ show (oId bi) ++ ")"
    show (SplitEdge bi)     = "split (" ++ show (oId bi) ++ ")"

instance (Show i, Show r) => Show (OGEdgeLabel i r) where
    show (DataFlowEdge o)        = "dataflow (" ++ show o ++ ")"
    show (OperandCopyEdge bi)    = "copy (" ++ show (oId bi) ++ ")"
    show (OperandNaturalEdge bi) = "natural (" ++ show (oId bi) ++ ")"
    show OperandCongruenceEdge   = "congruence"

showInstrs o = showInstructions [(o, inst) | inst <- oOprInstructions o]

showInstructions [inst] = showInstruction inst
showInstructions insts  = showAlternatives showInstruction insts

showInstruction (o, inst)
  | isVirtualInstruction inst || isBarrierInstruction inst =
    "(" ++ showVirtualOpc o ++ ")"
  | otherwise = show inst

showAlternatives f ls = "{" ++ renderStyle lineStyle (cs f ls) ++ "}"

showUs :: Show r => Operation i r -> String
showUs = show . oOprUses

showDs :: Show r => Operation i r -> String
showDs = show . oOprDefs

instance Show BlockAttributes where
    show (BlockAttributes {aEntry = entry, aExit = exit, aReturn = return,
                           aFreq = freq, aSplit = split}) =
        let attrs = mapMaybe blockAttrToMaybe
                    ([(entry, "entry"), (exit, "exit"), (return, "return")] ++
                     freqToTuple freq ++ [(split, "split")])
        in (if null attrs then ""
            else " (" ++ render (cs id attrs) ++ ")")

blockAttrToMaybe (True, attr) = Just attr
blockAttrToMaybe (False, _)   = Nothing

freqToTuple (Just f) = [(True, "freq: " ++ show f)]
freqToTuple Nothing  = []

instance (Show i, Show r) => Show (Attributes i r) where
    show (Attributes {aReads = reads, aWrites = writes, aCall = call,
                      aMem = mem, aActivators = act, aVirtualCopy = vc,
                      aRemat = rm, aJTBlocks = jtbs, aBranchTaken = bt,
                      aPrescheduled = ps, aRematOrigin = rorig,
                      aSplitBarrier = sb}) =
        let attrs = catMaybes
                    [maybeShowReads reads, maybeShowWrites writes,
                     maybeShowCall call, maybeShowMem mem,
                     maybeShowActivators act, maybeShowVirtualCopy vc,
                     maybeShowRemat rm, maybeShowJTBlocks jtbs,
                     maybeShowBranchTaken bt, maybeShowPrescheduled ps,
                     maybeShowRematOrigin rorig, maybeShowSplitBarrier sb]
        in (if null attrs then ""
            else " (" ++ render (cs id attrs) ++ ")")

maybeShowReads [] = Nothing
maybeShowReads rwos = Just $ showAttr "reads" "" rwos

maybeShowWrites [] = Nothing
maybeShowWrites rwos = Just $ showAttr "writes" "" rwos

maybeShowCall Nothing = Nothing
maybeShowCall (Just id) = Just $ showAttr "call" "o" id

maybeShowMem Nothing = Nothing
maybeShowMem (Just m) = Just $ showAttr "mem" "" m

maybeShowActivators [] = Nothing
maybeShowActivators insts = Just $ showAttr "activators" "" insts

maybeShowVirtualCopy False = Nothing
maybeShowVirtualCopy True = Just $ "virtualcopy"

maybeShowRemat False = Nothing
maybeShowRemat True = Just $ "remat"

maybeShowJTBlocks [] = Nothing
maybeShowJTBlocks jtbs = Just $ showAttr "jtblocks" "" (map SimpleBlockRef jtbs)

maybeShowBranchTaken Nothing = Nothing
maybeShowBranchTaken (Just bt) = Just $ "taken: " ++ showBool bt

showBool True  = "true"
showBool False = "false"

maybeShowPrescheduled Nothing = Nothing
maybeShowPrescheduled (Just c) = Just $ showAttr "cycle" "" c

maybeShowRematOrigin Nothing = Nothing
maybeShowRematOrigin (Just oid) = Just $ showAttr "remat-origin" "o" oid

maybeShowSplitBarrier False = Nothing
maybeShowSplitBarrier True = Just $ "split-barrier"

showAttr :: Show a => String -> String -> a -> String
showAttr n p a = n ++ ": " ++ p ++ show a

instance Show r => Show (RWObject r) where
    show (Memory o)               = "mem-" ++ o
    show AllMemory                = "mem"
    show ControlSideEffect        = "control"
    show ProgramCounterSideEffect = "pc"
    show (OtherSideEffect r)      = show r

instance Show r => Show (Operand r) where
    show (Temporary t r)          = 't' : show t ++ maybe "" showPreAssignment r
    show (BlockRef l)             = "b" ++ show l
    show (Register r)             = show r
    show (Bound e)                = showMachineOperand LLVM5 e
    show NullTemporary            = "-"
    show (MOperand i ts r) = "p" ++ show i ++ showAlternatives show ts
                                    ++ maybe "" showPreAssignment r
    show (OperandRef p)           = 'p' : show p

showPreAssignment r = ":" ++ show r

showSectionName = (++ ":")

showCongruences [] _ _ = ""
showCongruences ts l w =
    renderStyle (st l) (nest w (cs showCongruence ts)) ++ newLine

showCongruence (t, t') = show t ++ " -> " ++ show t'

showRematerializable [] _ _ = ""
showRematerializable ts l w =
  renderStyle (st l) (nest w (cs showRematTuple ts)) ++ newLine

showRematTuple (t, oids) =
  show t ++ " [" ++ render (cs showOperationId oids) ++ "]"

showOperationId oid = "o" ++ show oid

showFrameObjects _ []    = ""
showFrameObjects fixed fobjs =
  concat [wsString ++ showFrameObject fixed fobj ++ newLine | fobj <- fobjs]

showJumpTableEntries (_, []) = ""
showJumpTableEntries (k, es) =
    wsString ++ "kind: " ++ k ++ newLine ++
    concat [wsString ++ show e ++ newLine | e <- es]

showLineInst w i = wsString ++ showBlockOperation w i ++ newLine

instance Show r => Show (FrameObject r) where
    show = showFrameObject False

showFrameObject fixed fo =
    "%" ++ (if fixed then "fixed-" else "") ++ "stack." ++
        show (foIndex fo) ++ ": " ++
        renderStyle lineStyle
        (cs showFrameObjectProperty
         ([("offset", show (foOffset fo))] ++
           (case foSize fo of
             Just s -> [("size", show s)]
             Nothing -> []) ++
          [("align", show (foAlignment fo))] ++
           (case foCSRegister fo of
             Just r -> [("callee-saved-register", show r)]
             Nothing -> [])))

showFrameObjectProperty (p, v) = p ++ " = " ++ v

showConstantObjects [] = ""
showConstantObjects consts =
  concat [wsString ++ showConstantObject c ++ newLine | c <- consts]

showConstantObject (id, v, a) =
  "%const." ++ show id ++ ": value = " ++ showConstantValue v ++ ", align = " ++
  show a

instance Show JumpTableEntry where
    show e = jumpTablePrefix ++ show (jtId e) ++ ": " ++
             show (map SimpleBlockRef (jtBlocks e))

data SimpleBlockRef = SimpleBlockRef BlockId

instance Show SimpleBlockRef where
    show (SimpleBlockRef l) = "b" ++ show l

maxIdWidth :: Show i => [BlockOperation i r] -> Int
maxIdWidth = maximum . map (length . show . firstId)

firstId (SingleOperation {oId = id}) = id
firstId (Bundle {bundleOs = (i:_)}) = firstId i

instance Read HighLevelGoal where
  readsPrec _ str = [(readHighLevelGoal str, "")]

readHighLevelGoal "speed" = Speed
readHighLevelGoal "size"  = Size
readHighLevelGoal "spill" = Spill
readHighLevelGoal goal = error ("unmatched: readHighLevelGoal " ++ show goal)
