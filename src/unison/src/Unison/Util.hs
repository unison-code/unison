{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Useful functions to manipulate and query the Unison program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE FlexibleContexts #-}
module Unison.Util
       (
        flatten,
        flatCode,
        tOps,
        tUses,
        tDefs,
        temporaries,
        defTemporaries,
        codeAltTemps,
        mapToOperationInBlocks,
        mapToOperationId,
        mapToModelOperand,
        applyMapToOperands,
        applyTempIdMap,
        applyMOperandIdMap,
        renameOperands,
        sortOut,
        sortIn,
        regs,
        newBlockIndex,
        newTempIndex,
        newOprIndex,
        newOperIndex,
        newIndexes,
        mapToOperands,
        mapToOperandIf,
        mapToOprOperands,
        mapToInstructions,
        mapToVirtualOprInstructions,
        mapToReads,
        mapToWrites,
        mapToAttrCall,
        mapToAttrMem,
        mapToActivators,
        mapToAttrVirtualCopy,
        mapToAttrRemat,
        mapToAttrJTBlocks,
        mapToAttrRematOrigin,
        isTailCallFun,
        isTerminator,
        callOf,
        blockSucc,
        successors,
        exitBlocks,
        exitBlockIds,
        returnBlocks,
        returnBlockIds,
        findBlock,
        addLongLifeRegs,
        isEntryTemp,
        isExitTemp,
        entryIn,
        blockIn,
        blockOut,
        blockFreq,
        updateBlockFreq,
        normalize,
        addOperands,
        sortOperands,
        applyToBlock,
        applyToBlockCode,
        filterCode,
        entryBlock,
        mapToBlock,
        mapToEntryBlock,
        addToIn,
        addToOut,
        appendToIn,
        appendToOut,
        tUniqueOps,
        oModelOps,
        extractTemps,
        insertWhen,
        moveOperation,
        moveOperations,
        moveGloballyOperation,
        isIdOf,
        insertOperationInBlock,
        newId,
        before,
        after,
        users,
        definer,
        potentialUsers,
        potentialDefiner,
        phiUses,
        codeCongruences,
        combinePreAssignments,
        mergeUsages,
        mergeUsagesList,
        isUser,
        isDefiner,
        isPotentialUser,
        isPotentialDefiner,
        isEquivalentTo,
        isMandatory,
        isMandNaturalWith,
        accessType,
        isReadOf,
        isWriteOf,
        preAssignTemps,
        preAssign,
        undoPreAssign,
        preAssignment,
        preAssignments,
        toCongruenceOp,
        tempBlock,
        tempOperand,
        isAugmented,
        mapToOperation,
        mapToOperationInBlock,
        pragmas,
        pragmaHeader,
        splitPragmaComment,
        foldWithTempIndex,
        foldFunction,
        foldBlock,
        peephole,
        foldMatch,
        makeOptional,
        addNullTemp,
        foldVirtualCopy,
        cleanRedundantReads,
        -- * Accessors
        oAllOperands,
        oUseOperands,
        oDefOperands,
        oAllOps,
        oUses,
        oDefs,
        oOprUses,
        oOprDefs,
        oSingleUse,
        oSingleDef,
        copySource,
        copyDestination,
        copyOps,
        targetInst,
        oInstructions,
        oOprInstructions,
        oAnnInstructions,
        oType,
        oReadObjects,
        oWriteObjects,
        oRWObjects,
        oActivators,
        foMaybeSize,
        newFrameIndex,
        applyToLatency,
        -- * Comparators
        alternativeCopies,
        -- * Conversion functions
        fromMachineInstruction,
        splitMachineOperands,
        toMachineFunction,
        toMachineInstruction,
        toMachineOperand,
        toLinear,
        showVirtualOpc,
        toVirtualType,
        isDelimiterOperand,
        linearizeCode,
        linearizeOpr,
        toSubRegIndex,
        lowerGoal
       )
       where

import Data.List
import Data.Ord
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow

import Common.Util

import MachineIR.Base
import MachineIR.Constructors
import MachineIR.Predicates
import MachineIR.Instances ()

import Unison.Base
import Unison.Predicates
import Unison.Constructors

flatten :: [Block i r] -> [BlockOperation i r]
flatten = concatMap bCode . linearizeCode

flatCode ::  Function i r -> [BlockOperation i r]
flatCode = flatten . fCode

-- | Function that maps to a block operation
type BlockOperationMap i r = (BlockOperation i r -> BlockOperation i r)

mapToOperationInBlocks ::  BlockOperationMap i r -> [Block i r] -> [Block i r]
mapToOperationInBlocks = map . (applyToBlockCode . map)

-- | Function that maps to an operand
type OperandMap r = (Operand r -> Operand r)

-- | Predicate on an operand
type OperandPredicate r = (Operand r -> Bool)

mapToOperandIf ::  OperandPredicate r -> OperandMap r -> BlockOperation i r -> BlockOperation i r
mapToOperandIf p f = mapToOperands (mapIf p f) (mapIf p f)

applyToBlock :: BlockOperationsMap i r -> [Block i r] -> BlockId -> [Block i r]
applyToBlock f (b @ Block {bLab = l', bCode = code} : bs) l
    | l == l'   = b {bCode = f code} : bs
    | otherwise = b : applyToBlock f bs l

filterBlock :: BlockOperationPredicate i r -> Block i r -> Block i r
filterBlock f b @ Block {bCode = os} =
  b {bCode = concatMap (filterOperation f) os}

filterOperation :: BlockOperationPredicate i r -> BlockOperation i r -> [BlockOperation i r]
filterOperation f o @ Bundle {bundleOs = os} =
  case filter f os of
    []  -> []
    os' -> [o {bundleOs = os'}]
filterOperation f o @ SingleOperation {} = filter f [o]

-- | Predicate on a block operation
type BlockOperationPredicate i r = (BlockOperation i r -> Bool)

filterCode :: BlockOperationPredicate i r -> [Block i r] -> [Block i r]
filterCode = map . filterBlock

-- | Function that maps to a block operation identifier
type OperationIdMap = (OperationId -> OperationId)

mapToOperationId :: OperationIdMap -> BlockOperation i r -> BlockOperation i r
mapToOperationId f i @ (SingleOperation {oId = id, oAs = as}) =
  i {oId = f id, oAs = mapToOperationIdInAttributes f as}

mapToOperationIdInAttributes f
  as @ Attributes {aCall = id1, aRematOrigin = ro} =
    as {aCall = fmap f id1, aRematOrigin = fmap f ro}

mapToModelOperand :: OperandMap r -> BlockOperation i r -> BlockOperation i r
mapToModelOperand = mapToOperandIf isModelOperand

applyMapToOperands :: Ord r => M.Map (Operand r) (Operand r) ->
                      BlockOperation i r -> BlockOperation i r
applyMapToOperands = mapToOperandIf always . applyMap

applyTempIdMap :: M.Map TemporaryId TemporaryId -> Operand r -> Operand r
applyTempIdMap tMap t @ MOperand {altTemps = ts} =
  t {altTemps = map (applyTempIdMap tMap) ts}
applyTempIdMap tMap t @ Temporary {tId = old} =
  case M.lookup old tMap of
    Nothing -> t
    Just id -> t {tId = id}
applyTempIdMap _ o = o

applyMOperandIdMap :: M.Map MoperandId MoperandId -> Operand r -> Operand r
applyMOperandIdMap tMap p @ MOperand {operandId = old} =
  case M.lookup old tMap of
    Nothing -> p
    Just id -> p {operandId = id}
applyMOperandIdMap tMap p @ (OperandRef old) =
  case M.lookup old tMap of
    Nothing -> p
    Just id -> mkOperandRef id
applyMOperandIdMap _ o = o

renameOperands :: Ord r => ([BlockOperation i r] -> [Integer]) ->
                  (M.Map Integer Integer -> Operand r -> Operand r) ->
                  Function i r -> Function i r
renameOperands o r f @ Function {fCode = code, fCongruences = cs,
                                 fRematerializable = rts} =
  let o2n   = M.fromList (zip (nub $ o $ flatten code) [0..])
      rf    = r o2n
      code' = mapToOperationInBlocks (mapToModelOperand rf) code
      cs'   = sort $ map (mapTuple rf) cs
      rts'  = sort $ map (first rf) rts
  in f {fCode = code', fCongruences = cs', fRematerializable = rts'}

mapToReads :: ([RWObject r] -> [RWObject r]) -> BlockOperation i r ->
              BlockOperation i r
mapToReads f o @ SingleOperation {
               oAs = as @ Attributes {aReads = reads}} =
  o {oAs = as {aReads = f reads}}

mapToWrites :: ([RWObject r] -> [RWObject r]) -> BlockOperation i r ->
               BlockOperation i r
mapToWrites f o @ SingleOperation {
               oAs = as @ Attributes {aWrites = writes}} =
  o {oAs = as {aWrites = f writes}}

mapToAttrCall :: (Maybe OperationId -> Maybe OperationId) ->
                 BlockOperation i r -> BlockOperation i r
mapToAttrCall f o @ SingleOperation {
  oAs = as @ Attributes {aCall = call}} = o {oAs = as {aCall = f call}}

mapToAttrMem :: (Maybe Integer -> Maybe Integer) ->
                 BlockOperation i r -> BlockOperation i r
mapToAttrMem f o @ SingleOperation {
  oAs = as @ Attributes {aMem = mem}} = o {oAs = as {aMem = f mem}}

mapToActivators :: InstructionsMap i -> BlockOperation i r -> BlockOperation i r
mapToActivators f o @ SingleOperation {
               oAs = as @ Attributes {aActivators = insts}} =
  o {oAs = as {aActivators = f insts}}

mapToAttrVirtualCopy :: (Bool -> Bool) -> BlockOperation i r -> BlockOperation i r
mapToAttrVirtualCopy f o @ SingleOperation {
               oAs = as @ Attributes {aVirtualCopy = vc}} =
  o {oAs = as {aVirtualCopy = f vc}}

mapToAttrRemat :: (Bool -> Bool) -> BlockOperation i r -> BlockOperation i r
mapToAttrRemat f o @ SingleOperation {
               oAs = as @ Attributes {aRemat = rm}} =
  o {oAs = as {aRemat = f rm}}

mapToAttrJTBlocks :: ([BlockId] -> [BlockId]) -> BlockOperation i r -> BlockOperation i r
mapToAttrJTBlocks f o @ SingleOperation {
               oAs = as @ Attributes {aJTBlocks = bs}} =
  o {oAs = as {aJTBlocks = f bs}}

mapToAttrRematOrigin ::  (Maybe OperationId -> Maybe OperationId) ->
                          BlockOperation i r -> BlockOperation i r
mapToAttrRematOrigin f o @ SingleOperation {
  oAs = as @ Attributes {aRematOrigin = ro}} =
  o {oAs = as {aRematOrigin = f ro}}

isTailCallFun :: [BlockOperation i r] -> BlockOperation i r -> Bool
isTailCallFun code i
  | isFun i = isTailCall (callOf i code)
  | otherwise = False

isTerminator :: [BlockOperation i r] -> BlockOperation i r -> Bool
isTerminator code o = isBranch o || isTailCallFun code o

callOf :: BlockOperation i r -> [BlockOperation i r] -> BlockOperation i r
callOf SingleOperation {oAs = Attributes {aCall = Just c}}
  code = fromJust $ find (isId c) code

-- | Gives the successor blocks
blockSucc :: (BlockOperation i r -> Maybe BranchInfo) -> BlockId -> Block i r ->
             [BlockId]
blockSucc bif lastBB b @ Block {bLab = l, bCode = code}
  | isExitBlock b = []
  | otherwise =
    (case find isBranch (concatMap linearizeOpr code) of
        Just j  -> successors bif (l + 1) j
        Nothing -> if l == lastBB then [] else [l + 1])

-- | Gives the successor blocks of a branch operation with fallthrough block
-- indexed by 'ftb'
successors :: (BlockOperation i r -> Maybe BranchInfo) -> BlockId ->
              BlockOperation i r -> [BlockId]
successors bif ftb Bundle {bundleOs = os} =
  nub $ concatMap (successors bif ftb) os
successors bif ftb o
  | isBranch o =
    let jtbs  = aJTBlocks $ oAs o
        succs =
          case fromJust (bif o) of
            (BranchInfo Unconditional (Just l)) -> [l]
            (BranchInfo Conditional   (Just l)) -> [l, ftb]
            (BranchInfo Unconditional Nothing) ->  jtbs
            (BranchInfo Conditional   Nothing) ->  jtbs ++ [ftb]
    in nub succs
  | otherwise = []

exitBlocks ::  [Block i r] -> [Block i r]
exitBlocks = filter isExitBlock
exitBlockIds ::  [Block i r] -> [BlockId]
exitBlockIds = map bLab . exitBlocks

returnBlocks ::  [Block i r] -> [Block i r]
returnBlocks   = filter isReturnBlock
returnBlockIds ::  [Block i r] -> [BlockId]
returnBlockIds = map bLab . returnBlocks

findBlock ::  [Block i r] -> BlockId -> Block i r
findBlock code l = findBy l bLab code

addLongLifeRegs :: Ord r => ([Block i r] -> [BlockId]) -> [Operand r] ->
                   Function i r -> Function i r
addLongLifeRegs bf regs f @ Function {fCode = code} =
  let delimiterOps f code b = oAllOps $ f $ findBlock code b
      inOperands  = delimiterOps blockIn
      outOperands = delimiterOps blockOut
      inB    = bLab $ entryBlock code
      outBs  = bf code
      ops    = inOperands code inB : map (outOperands code) outBs
      brs    = [S.fromList (concat [mapMaybe preAssignment ps]) | ps <- ops]
      ers    = common brs
      rs     = regs \\ S.toList ers
      code'  = mapToEntryBlock (appendToIn rs) code
      code'' = foldl (applyToBlock (appendToOut rs)) code' outBs
    in f {fCode = code''}

common :: Ord a => [S.Set a] -> S.Set a
common sets = S.filter (\e -> all (S.member e) sets) (S.unions sets)

isEntryTemp ::  [Block i r] -> Operand r -> Bool
isEntryTemp code t = isDefiner t (blockIn (entryBlock code))
isExitTemp ::  [Block i r] -> Operand r -> Bool
isExitTemp  code t = any id [isUser t (blockOut b) | b <- exitBlocks code]

entryIn ::  [Block i r] -> BlockOperation i r
entryIn = blockIn . entryBlock

blockIn ::  Block i r -> BlockOperation i r
blockIn  = head . bCode
blockOut ::  Block i r -> BlockOperation i r
blockOut = last . bCode

entryBlock ::  [Block i r] -> Block i r
entryBlock = fromJust . find isEntryBlock

blockFreq ::  Block i r -> Frequency
blockFreq = fromJust . aFreq . bAs

updateBlockFreq :: (Frequency, Block i r) -> Block i r
updateBlockFreq (freq, b @ Block {bAs = attrs}) =
  b {bAs = (attrs {aFreq = Just freq})}

normalize ::  Integral b => [b] -> [b]
normalize rawfreq =
  let base = foldl gcd 0 rawfreq
  in map (flip div base) rawfreq

addOperands ::  [Operand r] -> [Operand r] -> BlockOperation i r -> BlockOperation i r
addOperands us ds = mapToOperands ((++) us) ((++) ds)

sortOperands ::  Ord r => [Operand r] -> [Operand r] -> BlockOperation i r ->
                 BlockOperation i r
sortOperands us ds = mapToOperands (concatAndSort us) (concatAndSort ds)

-- | Function that maps to a list of operands
type OperandsMap r = ([Operand r] -> [Operand r])

mapToOperands :: OperandsMap r -> OperandsMap r -> BlockOperation i r -> BlockOperation i r
mapToOperands f g bi @ SingleOperation {oOpr = o} =
  bi {oOpr = mapToOprOperands f g o}
mapToOperands f g bu @ Bundle {bundleOs = os} =
  bu {bundleOs = map (mapToOperands f g) os}

mapToOprOperands :: OperandsMap r -> OperandsMap r -> Operation i r -> Operation i r
mapToOprOperands f g (Natural o) = Natural (mapToNaturalOprOperands f g o)
mapToOprOperands f g (Virtual o) = Virtual (mapToVirtualOprOperands f g o)
mapToOprOperands f g o @ Copy {oCopyS = u, oCopyUs = us, oCopyD = d, oCopyDs = ds} =
  o {oCopyS = mapSingle f u, oCopyUs = f us, oCopyD = mapSingle g d, oCopyDs = g ds}

mapToNaturalOprOperands :: OperandsMap r -> OperandsMap r -> NaturalOperation i r -> NaturalOperation i r
mapToNaturalOprOperands f g o @ Linear {oUs = us, oDs = ds} =
  o {oUs = f us, oDs = g ds}
mapToNaturalOprOperands f _ o @ Branch {oBranchUs = us} = o {oBranchUs = f us}
mapToNaturalOprOperands f _ o @ Call {oCallUs = cUs} = o {oCallUs = f cUs}
mapToNaturalOprOperands f _ o @ TailCall {oTailCallUs = tcUs} =
  o {oTailCallUs = f tcUs}

mapToVirtualOprOperands :: OperandsMap r -> OperandsMap r -> VirtualOperation r -> VirtualOperation r
mapToVirtualOprOperands f g o @ Phi {oPhiUs = us, oPhiD = d} =
  o {oPhiUs = f us, oPhiD = mapSingle g d}
mapToVirtualOprOperands f g (Delimiter o) =
  Delimiter (mapToDelimiterOprOperands f g o)
mapToVirtualOprOperands f _ o @ Kill {oKillUs = us} = o {oKillUs = f us}
mapToVirtualOprOperands _ g o @ Define {oDefineDs = ds} = o {oDefineDs = g ds}
mapToVirtualOprOperands f g o @ Combine {oCombineLowU = lu, oCombineHighU = hu,
                                          oCombineD = d} =
  o {oCombineLowU  = mapSingle f lu,
     oCombineHighU = mapSingle f hu,
     oCombineD     = mapSingle g d}
mapToVirtualOprOperands f g o @ Low {oLowU = u, oLowD = d} =
  o {oLowU = mapSingle f u, oLowD = mapSingle g d}
mapToVirtualOprOperands f g o @ High {oHighU = u, oHighD = d} =
  o {oHighU = mapSingle f u, oHighD = mapSingle g d}
mapToVirtualOprOperands f g o @ Split2 {oSplit2U = u, oSplit2LowD = ld,
                                        oSplit2HighD = hd} =
  o {oSplit2U = mapSingle f u,
     oSplit2LowD = mapSingle g ld, oSplit2HighD = mapSingle g hd}
mapToVirtualOprOperands f g o @ Split4 {
  oSplit4U = u, oSplit4LowLowD = lld, oSplit4LowHighD = lhd,
  oSplit4HighLowD = hld, oSplit4HighHighD = hhd} =
  o {oSplit4U = mapSingle f u, oSplit4LowLowD = mapSingle g lld,
     oSplit4LowHighD = mapSingle g lhd, oSplit4HighLowD = mapSingle g hld,
     oSplit4HighHighD = mapSingle g hhd}
mapToVirtualOprOperands f g
  o @ VirtualCopy {oVirtualCopyS = u, oVirtualCopyD = d} =
  o {oVirtualCopyS = mapSingle f u, oVirtualCopyD = mapSingle g d}
mapToVirtualOprOperands f g o @ Fun {oFunctionUs = us, oFunctionDs = ds} =
  o {oFunctionUs = f us, oFunctionDs = g ds}
mapToVirtualOprOperands f g (Frame o) = Frame (mapToFrameOprOperands f g o)

mapToDelimiterOprOperands :: OperandsMap r -> OperandsMap r -> DelimiterOperation r -> DelimiterOperation r
mapToDelimiterOprOperands _ g o @ In {oIns = ds} = o {oIns = g ds}
mapToDelimiterOprOperands f _ o @ Out {oOuts = us} = o {oOuts = f us}
mapToDelimiterOprOperands _ g o @ Entry {oEntry = ds} = o {oEntry = g ds}
mapToDelimiterOprOperands f _ o @ Return {oReturn = us} = o {oReturn = f us}
mapToDelimiterOprOperands _ _ o @ Exit {} = o

mapToFrameOprOperands :: OperandsMap r -> OperandsMap r -> FrameOperation r -> FrameOperation r
mapToFrameOprOperands f _ o @ Setup {oSetupU = u} = o {oSetupU = mapSingle f u}
mapToFrameOprOperands f _ o @ Destroy {oDestroyU = u} =
    o {oDestroyU = mapSingle f u}

-- | Function that maps to a list of instructions
type InstructionsMap i = ([Instruction i] -> [Instruction i])

mapToInstructions :: InstructionsMap i -> BlockOperation i r -> BlockOperation i r
mapToInstructions f bi @ SingleOperation {oOpr = o} =
  bi {oOpr = mapToOprInstructions f o}

mapToOprInstructions :: InstructionsMap i -> Operation i r -> Operation i r
mapToOprInstructions f (Natural o) = Natural (mapToNaturalOprInstructions f o)
mapToOprInstructions f o @ Copy {oCopyIs = is} = o {oCopyIs = f is}
mapToOprInstructions f (Virtual o) =
    Virtual (mapToVirtualOprInstructions f o)

mapToNaturalOprInstructions :: InstructionsMap i -> NaturalOperation i r -> NaturalOperation i r
mapToNaturalOprInstructions f o @ Linear {oIs = is} = o {oIs = f is}
mapToNaturalOprInstructions f o @ Branch {oBranchIs = is} = o {oBranchIs = f is}
mapToNaturalOprInstructions f o @ Call {oCallIs = is} = o {oCallIs = f is}
mapToNaturalOprInstructions f o @ TailCall {oTailCallIs = is} = o {oTailCallIs = f is}

mapToVirtualOprInstructions :: InstructionsMap i -> VirtualOperation r -> VirtualOperation r
mapToVirtualOprInstructions f o @ Kill {oKillIs = is} = o {oKillIs = applyToGeneral f is}
mapToVirtualOprInstructions f o @ Low {oLowIs = is} = o {oLowIs = applyToGeneral f is}
mapToVirtualOprInstructions f o @ High {oHighIs = is} = o {oHighIs = applyToGeneral f is}
mapToVirtualOprInstructions _ o = o

applyToGeneral :: InstructionsMap i -> [GeneralInstruction] -> [GeneralInstruction]
applyToGeneral f is =
    let gis  = map General is
        gis' = f gis
    in map oGeneralInstr gis'

mapSingle :: ([a] -> [b]) -> a -> b
mapSingle f s = fromSingleton $ f [s]

-- | Function that maps to a list of block operations
type BlockOperationsMap i r = ([BlockOperation i r] -> [BlockOperation i r])

mapToBlock ::  BlockOperationsMap i r -> BlockId -> [Block i r] -> [Block i r]
mapToBlock f l = mapIf (\b -> bLab b == l) (applyToBlockCode f)
mapToEntryBlock ::  BlockOperationsMap i r -> [Block i r] -> [Block i r]
mapToEntryBlock f = mapIf isEntryBlock (applyToBlockCode f)

applyToBlockCode :: BlockOperationsMap i r -> Block i r -> Block i r
applyToBlockCode f b @ Block {bCode = code} = b {bCode = f code}

addToIn :: Ord r => [Operand r] -> [BlockOperation i r] -> [BlockOperation i r]
addToIn = mapToIn addNewOperands
addToOut :: Ord r => [Operand r] -> [BlockOperation i r] -> [BlockOperation i r]
addToOut = mapToOut addNewOperands

addNewOperands :: Ord r => [Operand r] -> [Operand r] -> BlockOperation i r ->
                  BlockOperation i r
addNewOperands us ds = mapToOperands (appendUnique us) (appendUnique ds)

concatAndSort :: Ord a => [a] -> [a] -> [a]
concatAndSort a b = sort (a ++ b)

appendUnique :: Ord a => [a] -> [a] -> [a]
appendUnique a b =
  let b' = [e | e <- b, not (e `elem` a)]
  in concatAndSort a b'

appendToIn :: Eq r => [Operand r] -> [BlockOperation i r] ->
              [BlockOperation i r]
appendToIn = mapToIn appendNewOperands
appendToOut :: Eq r => [Operand r] -> [BlockOperation i r] ->
               [BlockOperation i r]
appendToOut = mapToOut appendNewOperands

appendNewOperands :: Eq r => [Operand r] -> [Operand r] -> BlockOperation i r ->
                     BlockOperation i r
appendNewOperands us ds =
  let appendNew a b = b ++ (a \\ b)
  in mapToOperands (appendNew us) (appendNew ds)

sortIn :: Ord r => [BlockOperation i r] -> [BlockOperation i r]
sortIn = mapToIn sortOperands []
sortOut :: Ord r => [BlockOperation i r] -> [BlockOperation i r]
sortOut = mapToOut sortOperands []

mapToIn f ds (vIn:code) = f [] ds vIn : code
mapToOut f us code = init code ++ [f us [] (last code)]

newBlockIndex ::  [Block i r] -> BlockId
newBlockIndex [] = 0
newBlockIndex code = maximum (map bLab code) + 1

newTempIndex ::  [BlockOperation i r] -> TemporaryId
newTempIndex code =
    let ts = sort (temporaries code)
    in if null ts then 0 else last ts + 1

newOprIndex ::  [BlockOperation i r] -> OperationId
newOprIndex code =
    let os = sortBy (comparing oId) code
    in if null os then 0 else oId (last os) + 1

newOperIndex :: Eq r => [BlockOperation i r] -> MoperandId
newOperIndex code =
    let ps = sort $ map operandId $ concatMap oAllOperands code
    in if null ps then 0 else last ps + 1

newIndexes :: Eq r => [BlockOperation i r] ->
              (TemporaryId, OperationId, MoperandId)
newIndexes code = (newTempIndex code, newOprIndex code, newOperIndex code)

newId ::  [Block i r] -> OperationId
newId = newOprIndex . flatten

ops = filterOps oAllOps
useOps = filterOps oUses
defOps = filterOps oDefs

filterOps f isTargetOp code =
  let ops = concatMap f code
  in filter isTargetOp ops

regs ::  [BlockOperation i r] -> [Operand r]
regs = ops isRegister

temporaries ::  [BlockOperation i r] -> [TemporaryId]
temporaries = map tId . tOps

defTemporaries :: [BlockOperation i r] -> [TemporaryId]
defTemporaries = map tId . concatMap oDefTemps

codeAltTemps ::  [BlockOperation i r] -> [MoperandId]
codeAltTemps = map operandId . concatMap (filter isMOperand . oAllOps)

tUses ::  [BlockOperation i r] -> [Operand r]
tUses = useOps isTemporary
tDefs ::  [BlockOperation i r] -> [Operand r]
tDefs = defOps isTemporary

tOps ::  [BlockOperation i r] -> [Operand r]
tOps = concatMap oTemps
tUniqueOps ::  Eq r => [BlockOperation i r] -> [Operand r]
tUniqueOps = nub . concatMap oTemps

oModelOps ::  BlockOperation i r -> [Operand r]
oModelOps = filter isModelOperand . oAllOps

oTemps :: BlockOperation i r -> [Operand r]
oTemps = concatMap extractTemps . oAllOps

oDefTemps = concatMap extractTemps . oDefs

extractTemps ::  Operand r -> [Operand r]
extractTemps t @ Temporary {} = [t]
extractTemps MOperand {altTemps = ts, operandReg = r} =
  let ts' = filter (not . isNullTemporary) ts
  in case r of
    (Just reg) -> map ((flip preAssign) reg) ts'
    Nothing    -> ts'
extractTemps _ = []

type BlockPosition i r = Splitter (BlockOperation i r) -> Splitter (BlockOperation i r)

insertWhen :: BlockPosition i r -> BlockOperationPredicate i r -> [BlockOperation i r] ->
              [BlockOperation i r] -> [BlockOperation i r]
insertWhen f g newCode code =
    let [preCode, postCode] = split (f $ whenElt g) code
    in preCode ++ newCode ++ postCode

moveOperation :: Eq i => Eq r => BlockOperation i r ->
                 BlockPosition i r -> BlockOperationPredicate i r ->
                 [BlockOperation i r] -> [BlockOperation i r]
moveOperation o f g = insertWhen f g [o] .  delete o

before ::  BlockPosition i r
before = keepDelimsL
after ::  BlockPosition i r
after = keepDelimsR

moveOperations :: Eq i => Eq r => BlockOperationPredicate i r ->
                  BlockPosition i r -> BlockOperationPredicate i r ->
                  Block i r -> Block i r
moveOperations isCandidate relPos isTargetInst b @ Block {bCode = code}
    | not (any isCandidate code) = b
    | otherwise =
      let candidates = filter isCandidate (reverse code)
          code'  = foldl (moveOpr relPos isTargetInst) code candidates
      in b {bCode = code'}

moveOpr :: Eq i => Eq r => BlockPosition i r -> BlockOperationPredicate i r
     -> [BlockOperation i r] -> BlockOperation i r -> [BlockOperation i r]
moveOpr relPos isTargetInst code o = moveOperation o relPos isTargetInst code

isIdOf ::  BlockOperation i r -> BlockOperation i r -> Bool
isIdOf = isId . oId

moveGloballyOperation ::  BlockOperation i r -> BlockPosition i r ->
                          BlockOperationPredicate i r -> [Block i r] -> [Block i r]
moveGloballyOperation o w f code =
    let code'  = filterCode (not . isIdOf o) code
        code'' = map (insertOperationInBlock w f o) code'
    in code''

insertOperationInBlock :: BlockPosition i r -> BlockOperationPredicate i r ->
                          BlockOperation i r -> Block i r -> Block i r
insertOperationInBlock w f o b @ Block {bCode = code} =
    let code' = if any f code then insertWhen w f [o] code else code
    in b {bCode = code'}

users ::  Operand r -> [BlockOperation i r] -> [BlockOperation i r]
users = filter . isUser
definer ::  Operand r -> [BlockOperation i r] -> BlockOperation i r
definer t = fromJust . find (isDefiner t)

potentialUsers ::  Operand r -> [BlockOperation i r] -> [BlockOperation i r]
potentialUsers = filter . isPotentialUser
potentialDefiner ::  Operand r -> [BlockOperation i r] -> BlockOperation i r
potentialDefiner t = fromJust . find (isPotentialDefiner t)

phiUses ::  BlockOperation i r -> [(Operand r, BlockId)]
phiUses o =
  let toBBLabel (BlockRef l) = l
  in [(u, toBBLabel l) | [u, l] <- chunksOf 2 $ oUses o]

codeCongruences :: Ord r => [BlockOperation i r] -> [(Operand r, Operand r)] ->
                   [(Operand r, Operand r)]
codeCongruences code cs =
  let ts = S.fromList $ tUniqueOps code
  in [(t, t') | (t, t') <- cs, S.member t ts && S.member t' ts]

type PreAssignTuple r = (OperationId, Operand r, Operand r)

type RegOperationIds r = (Operand r, [OperationId])

combinePreAssignments :: Ord r => [PreAssignTuple r] ->
                         [(Operand r, RegOperationIds r)]
combinePreAssignments = M.toList . M.fromListWith combinePas . pasToAList

combinePas :: Eq r => RegOperationIds r -> RegOperationIds r ->
              RegOperationIds r
combinePas (r, o) (r', o') | r == r' = (r, o ++ o')

pasToAList :: [(OperationId, Operand r, Operand r)] -> [(Operand r, RegOperationIds r)]
pasToAList pas = [(t, (r, [i])) | (i, t, r) <- pas]

mergeUsagesList (u:us) = foldl mergeUsages u us

mergeUsages :: Ord s => [Usage s] -> [Usage s] -> [Usage s]
mergeUsages u1 u2 =
  let [u1', u2'] = map usageMap [u1, u2]
      us         = M.toList $ M.fromListWith combineUsages (u1' ++ u2')
  in [Usage r us occ off | (r, (us, occ, off)) <- us]

type IntegerTriple = (Integer, Integer, Integer)

usageMap :: [Usage s] -> [(s, IntegerTriple)]
usageMap us = [(r, (us, occ, off)) | (Usage r us occ off) <- us]

combineUsages :: IntegerTriple -> IntegerTriple -> IntegerTriple
combineUsages (us, occ, off) (us', occ', off')
  | off /= off' = error ("usages cannot be combined, different offset")
  | occ == occ' = (us + us', occ,  off)
  | us  == us'  = (us, occ + occ', off)

accessType :: Eq r => RWObject r -> BlockOperation i r -> Maybe Access
accessType rwo o
    | isReadOf rwo o  = Just Read
    | isWriteOf rwo o = Just Write
    | otherwise       = Nothing

isReadOf :: Eq r => RWObject r -> BlockOperation i r -> Bool
isReadOf rwo i = rwo `elem` oReadObjects i
isWriteOf ::  Eq r => RWObject r -> BlockOperation i r -> Bool
isWriteOf rwo i = rwo `elem` oWriteObjects i

preAssignTemps :: Ord r => [(OperationId, Operand r, Operand r)] ->
                  [Block i r] -> [Block i r]
preAssignTemps pats code =
    let pas   = M.fromListWith (++) [(o, [(t, r)]) | (o, t, r) <- pats]
        code' = map (preAssignBlockTemps pas) code
    in code'

preAssignBlockTemps :: Ord r => M.Map OperationId [(Operand r, Operand r)] ->
                       Block i r -> Block i r
preAssignBlockTemps pas b = b {bCode = map (preAssignOprTemps pas) (bCode b)}

preAssignOprTemps :: Ord r => M.Map OperationId [(Operand r, Operand r)] ->
                     BlockOperation i r -> BlockOperation i r
preAssignOprTemps pas o @ SingleOperation {oId = id} =
    case M.lookup id pas of
      Nothing -> o
      Just ps -> mapToModelOperand (preAssignWith (M.fromList ps)) o

preAssignWith :: Ord r => M.Map (Operand r) (Operand r) -> Operand r ->
                 Operand r
preAssignWith ps t =
    case M.lookup t ps of
      Nothing -> t
      Just r  -> preAssign t r

preAssign ::  Operand r -> Operand r -> Operand r
preAssign t r = t {tReg = Just r}

undoPreAssign :: Operand r -> Operand r
undoPreAssign t @ Temporary {} = t {tReg = Nothing}
undoPreAssign t @ MOperand {}  = t {operandReg = Nothing}

preAssignment :: Operand r -> Maybe (Operand r)
preAssignment Temporary {tReg = r}      = r
preAssignment MOperand {operandReg = r} = r
preAssignment r @ Register {} = Just r
preAssignment _ = Nothing

preAssignments :: [Block i r] -> [PreAssignTuple r]
preAssignments = concatMap mkPreAssign
mkPreAssign :: Block i r -> [PreAssignTuple r]
mkPreAssign b = concatMap mkOprPreAssign (bCode b)

mkOprPreAssign :: BlockOperation i r -> [PreAssignTuple r]
mkOprPreAssign o = mapMaybe (mkOpPreAssign o) (oAllOps o)

mkOpPreAssign :: BlockOperation i r -> Operand r -> Maybe (PreAssignTuple r)
mkOpPreAssign o (Temporary t (Just r))  = Just (oId o, mkTemp t, r)
mkOpPreAssign o (MOperand p _ (Just r)) = Just (oId o, mkOperandRef p, r)
mkOpPreAssign _ _ = Nothing

toCongruenceOp :: Operand r -> Operand r
toCongruenceOp (Temporary t _) = mkTemp t
toCongruenceOp (MOperand p _ _) = mkOperandRef p

tempBlock ::  [Block i r] -> Operand r -> Block i r
tempBlock code t =
  let defined p b = any (isDefiner p) (bCode b)
  in fromJust $ find (defined t) code

tempOperand :: Eq r => Operand r -> BlockOperation i r -> Operand r
tempOperand t o =
  fromJust $ find (\p -> t `elem` extractTemps p) (oAllOperands o)

isAugmented ::  Function i r -> Bool
isAugmented f = any (any isMOperand . oAllOps) $ flatCode f

mapToOperation :: BlockOperationMap i r -> Function i r -> Function i r
mapToOperation f (fun @ Function {fCode = code}) =
  fun {fCode = map (mapToOperationInBlock f) code}

mapToOperationInBlock :: BlockOperationMap i r -> Block i r -> Block i r
mapToOperationInBlock f (b @ Block {bCode = code}) = b {bCode = map f code}

pragmas :: String -> [String] -> [String]
pragmas tool = concatMap (linePragmas tool)

pragmaHeader :: String -> String
pragmaHeader tool = "@" ++ tool ++ ":"

splitPragmaComment :: String -> [String]
splitPragmaComment = split (dropBlanks $ dropDelims $ oneOf " ")

linePragmas tool comment =
    case splitPragmaComment comment of
      header : pragmas -> if header == (pragmaHeader tool) then pragmas else []
      _ -> []

foldWithTempIndex f acc0 code =
  let i        = newTempIndex (flatten code)
      (_, acc) = foldl f (i, acc0) code
  in  acc

foldFunction fun acc f @ Function {fCode = code} =
  let (code', _) = foldl (foldBlock fun) ([], acc) code
  in f {fCode = code'}

foldBlock fun (accCode, acc) b @ Block {bCode = code} =
  let (code', acc') = foldl fun (code, acc) code
  in (accCode ++ [b {bCode = code'}], acc')

foldMatch :: ([BlockOperation i r] -> a -> ([BlockOperation i r], a)) ->
             a -> Function i r -> a
foldMatch f acc Function {fCode = code} = foldl (foldMatchBlock f) acc code

foldMatchBlock f acc Block {bCode = code} = applyFoldMatch f acc code

applyFoldMatch _ acc [] = acc
applyFoldMatch f acc code =
    let (code', acc') = f code acc
    in applyFoldMatch f acc' code'

makeOptional :: BlockOperation i r -> BlockOperation i r
makeOptional o =
  let o'  = addNullInstruction o
      o'' = mapToModelOperand addNullTemp o'
  in o''

addNullInstruction o
  | isNatural o = mapToInstructions (\is -> [mkNullInstruction] ++ is) o
addNullInstruction o @ SingleOperation {oOpr = Virtual opr} =
  o {oOpr = Virtual (mapToVirtualOprInstructions addNullInstr opr)}

addNullInstr is = [mkNullInstruction] ++ is

addNullTemp :: Operand r -> Operand r
addNullTemp p @ MOperand {altTemps = ts} = p {altTemps = [mkNullTemp] ++ ts}

foldVirtualCopy :: Eq i => Eq r =>
                   ([Block i r] -> BlockOperationPredicate i r) ->
                   Function i r -> Function i r
foldVirtualCopy p f @ Function {fCode = code, fCongruences = cs} =
  case find (p code) (flatten code) of
   Nothing -> f
   Just c ->
     let (d, s) = copyOps c
         code'  = filterCode (not . (==) c) code
         d2s    = M.fromList [(tId d, tId s)]
         code'' = mapToOperationInBlocks
                  (mapToModelOperand (applyTempIdMap d2s)) code'
         cs'    = map (mapTuple (applyTempIdMap d2s)) cs
     in f {fCode = code'', fCongruences = cs'}

cleanRedundantReads :: Eq r => BlockOperation i r -> BlockOperation i r
cleanRedundantReads o @ SingleOperation {} =
  let rs' = oReadObjects o \\ oWriteObjects o
  in mapToReads (const rs') o
cleanRedundantReads o @ Bundle {bundleOs = os} =
  o {bundleOs = map cleanRedundantReads os}

peephole :: Eq r => OperationTransform i r -> FunctionTransform i r
peephole tf f @ Function {fCode = code} =
  let ids        = newIndexes $ flatten code
      (_, code') = foldl (peepholeBlock (tf f)) (ids, []) code
  in f {fCode = code'}

peepholeBlock tf (ids, accCode) b @ Block {bCode = code} =
    let code' = applyPeephole tf code [] ids
        ids'  = updateIndexes ids code'
    in (ids', accCode ++ [b {bCode = code'}])

-- TODO: this is almost a foldl, the difference is that we can reduce more than
-- one element at a time, is there any abstraction for doing that?
applyPeephole _ [] tcode _ = tcode
applyPeephole tf rcode tcode ids =
    let (rest, tcode') = tf rcode ids
        ids' = updateIndexes ids tcode'
    in applyPeephole tf rest (tcode ++ tcode') ids'

updateIndexes (ti, ii, pi) code =
  let ti' = maxIndex ti (newTempIndex code)
      ii' = maxIndex ii (newOprIndex code)
      pi' = maxIndex pi (newOperIndex code)
  in (ti', ii', pi')

maxIndex id f = max id f + 1

-- Accessors

oUses :: BlockOperation i r -> [Operand r]
oUses (SingleOperation {oOpr = o}) = oOprUses o
oDefs :: BlockOperation i r -> [Operand r]
oDefs (SingleOperation {oOpr = o}) = oOprDefs o

oOprUses :: Operation i r -> [Operand r]
oOprUses (Natural (o @ Linear {})) = oUs o
oOprUses (Natural (o @ Branch {})) = oBranchUs o
oOprUses (Natural (o @ Call {})) = oCallUs o
oOprUses (Natural (o @ TailCall {})) = oTailCallUs o
oOprUses (Virtual (o @ Phi {})) = oPhiUs o
oOprUses (Virtual (Delimiter (In {}))) = []
oOprUses (Virtual (Delimiter (o @ Out {}))) = oOuts o
oOprUses (Virtual (Delimiter (Entry {}))) = []
oOprUses (Virtual (Delimiter (o @ Return {}))) = oReturn o
oOprUses (Virtual (Delimiter (Exit {}))) = []
oOprUses (Virtual (o @ Kill {})) = oKillUs o
oOprUses (Virtual (Define {})) = []
oOprUses (Virtual (o @ Combine {})) = [oCombineLowU o, oCombineHighU o]
oOprUses (Virtual (o @ Low {})) = [oLowU o]
oOprUses (Virtual (o @ High {})) = [oHighU o]
oOprUses (Virtual (o @ Split2 {})) = [oSplit2U o]
oOprUses (Virtual (o @ Split4 {})) = [oSplit4U o]
oOprUses (Virtual (o @ VirtualCopy {})) = [oVirtualCopyS o]
oOprUses (Virtual (o @ Fun {})) = oFunctionUs o
oOprUses (Virtual (Frame (o @ Setup {}))) = [oSetupU o]
oOprUses (Virtual (Frame (o @ Destroy {}))) = [oDestroyU o]
oOprUses o @ Copy {} = oCopyS o : oCopyUs o

oOprDefs :: Operation i r -> [Operand r]
oOprDefs (Natural (o @ Linear {})) = oDs o
oOprDefs (Natural Branch {}) = []
oOprDefs (Natural (Call {})) = []
oOprDefs (Natural (TailCall {})) = []
oOprDefs (Virtual (o @ Phi {})) = [oPhiD o]
oOprDefs (Virtual (Delimiter (o @ In {}))) = oIns o
oOprDefs (Virtual (Delimiter (Out {}))) = []
oOprDefs (Virtual (Delimiter (o @ Entry {}))) = oEntry o
oOprDefs (Virtual (Delimiter (Return {}))) = []
oOprDefs (Virtual (Delimiter (Exit {}))) = []
oOprDefs (Virtual (Kill {})) = []
oOprDefs (Virtual (o @ Define {})) = oDefineDs o
oOprDefs (Virtual (o @ Combine {})) = [oCombineD o]
oOprDefs (Virtual (o @ Low {})) = [oLowD o]
oOprDefs (Virtual (o @ High {})) = [oHighD o]
oOprDefs (Virtual (o @ Split2 {})) = [oSplit2LowD o, oSplit2HighD o]
oOprDefs (Virtual (o @ Split4 {})) = [oSplit4LowLowD o, oSplit4LowHighD o,
                                      oSplit4HighLowD o, oSplit4HighHighD o]
oOprDefs (Virtual (o @ VirtualCopy {})) = [oVirtualCopyD o]
oOprDefs (Virtual (o @ Fun {})) = oFunctionDs o
oOprDefs (Virtual (Frame (Setup {}))) = []
oOprDefs (Virtual (Frame (Destroy {}))) = []
oOprDefs o @ Copy {} = oCopyD o : oCopyDs o

oAllOps ::  BlockOperation i r -> [Operand r]
oAllOps o = oUses o ++ oDefs o

oAllOperands :: Eq r => BlockOperation i r -> [Operand r]
oAllOperands o = oUseOperands o ++ oDefOperands o
oUseOperands :: Eq r => BlockOperation i r -> [Operand r]
oUseOperands = uniqueOperands . oUses
oDefOperands :: Eq r => BlockOperation i r -> [Operand r]
oDefOperands = uniqueOperands . oDefs

uniqueOperands :: Eq r => [Operand r] -> [Operand r]
uniqueOperands = nub . filterOperands

filterOperands :: [Operand r] -> [Operand r]
filterOperands = filter isMOperand

oSingleUse :: BlockOperation i r -> Operand r
oSingleUse = fromSingleton . oUses
oSingleDef :: BlockOperation i r -> Operand r
oSingleDef = fromSingleton . oDefs

copySource :: BlockOperation i r -> Operand r
copySource      = head . oUses
copyDestination :: BlockOperation i r -> Operand r
copyDestination = head . oDefs

copyOps :: BlockOperation i r -> (Operand r, Operand r)
copyOps o = (copySource o, copyDestination o)

isUser :: Operand r -> BlockOperation i r -> Bool
isUser    t o = any (isEquivalentTo t) (oUses o)
isDefiner :: Operand r -> BlockOperation i r -> Bool
isDefiner t o = any (isEquivalentTo t) (oDefs o)

isPotentialUser :: Operand r -> BlockOperation i r -> Bool
isPotentialUser t o =
  any (isEquivalentTo t) $ concatMap extractTemps (oUses o)
isPotentialDefiner :: Operand r -> BlockOperation i r -> Bool
isPotentialDefiner t o =
  any (isEquivalentTo t) $ concatMap extractTemps (oDefs o)

isEquivalentTo :: Operand r -> Operand r -> Bool
isEquivalentTo Temporary {tId = t} Temporary {tId = t'} | t == t' = True
isEquivalentTo MOperand {altTemps = [Temporary {tId = t}]}
               MOperand {altTemps = [Temporary {tId = t'}]} = t == t'
isEquivalentTo MOperand {altTemps = [Temporary {tId = t}]}
               Temporary {tId = t'} = t == t'
isEquivalentTo Temporary {tId = t}
               MOperand {altTemps = [Temporary {tId = t'}]} = t == t'
isEquivalentTo _ _ = False

isMandatory ::  Eq i => BlockOperation i r -> Bool
isMandatory o = none isNullInstruction (oInstructions o)

-- | Predicate on a processor instruction
type TargetInstructionPredicate i = (i -> Bool)

isMandNaturalWith :: Eq i => TargetInstructionPredicate i ->
                     BlockOperation i r -> Bool
isMandNaturalWith p o =
  let is = oInstructions o
  in isNatural o && isMandatory o && all p (map oTargetInstr is)

isDelimiterOperand ::  [BlockOperation i r] -> Operand r -> Bool
isDelimiterOperand code t = any isDelimiter (definer t code : users t code)

targetInst ::  [Instruction i] -> i
targetInst = oTargetInstr . fromJust . find isTargetInstruction

oInstructions ::  BlockOperation i r -> [Instruction i]
oInstructions = oOprInstructions . oOpr

oOprInstructions ::  Operation i r -> [Instruction i]
oOprInstructions (o @ Copy {}) = oCopyIs o
oOprInstructions (Natural (o @ Linear {})) = oIs o
oOprInstructions (Natural (o @ Branch {})) = oBranchIs o
oOprInstructions (Natural (o @ Call {})) = oCallIs o
oOprInstructions (Natural (o @ TailCall {})) = oTailCallIs o
oOprInstructions (Virtual (o @ Kill {})) = map General (oKillIs o)
oOprInstructions (Virtual (o @ Low {})) = map General (oLowIs o)
oOprInstructions (Virtual (o @ High {})) = map General (oHighIs o)
oOprInstructions (Virtual Fun {}) = [mkBarrierInstruction]
oOprInstructions (Virtual Delimiter {}) = [mkBarrierInstruction]
oOprInstructions (Virtual _) = [mkVirtualInstruction]

oAnnInstructions :: BlockOperation i r -> [AnnotatedInstruction i]
oAnnInstructions o = [(instr, oType o) | instr <- oInstructions o]

oReadObjects ::  BlockOperation i r -> [RWObject r]
oReadObjects  = aReads . oAs
oWriteObjects ::  BlockOperation i r -> [RWObject r]
oWriteObjects = aWrites . oAs
oRWObjects ::  BlockOperation i r -> [RWObject r]
oRWObjects o = oReadObjects o ++ oWriteObjects o
oActivators ::  BlockOperation i r -> [Instruction i]
oActivators = aActivators . oAs

foMaybeSize :: FrameObject r -> Integer
foMaybeSize fo =
  case foSize fo of
    Just size -> size
    Nothing -> 0

newFrameIndex :: [FrameObject r] -> Integer
newFrameIndex []   = 0
newFrameIndex objs = maximum (map foIndex objs) + 1

applyToLatency :: (Latency -> Latency) -> OperandInfo rc -> OperandInfo rc
applyToLatency f ti @ TemporaryInfo {oiLatency = l} = ti {oiLatency = f l}
applyToLatency _ op = op

oType :: BlockOperation i r -> OperationT
oType (SingleOperation {oOpr = inst}) = oOprType inst

oOprType :: Operation i r -> OperationT
oOprType (Natural o) = NaturalType (oNaturalType o)
oOprType (Virtual o) = VirtualType (oVirtualType o)
oOprType (Copy {}) = CopyType

oNaturalType :: NaturalOperation i r -> NaturalT
oNaturalType Linear {}   = LinearType
oNaturalType Branch {}   = BranchType
oNaturalType Call   {}   = CallType
oNaturalType TailCall {} = TailCallType

oVirtualType :: VirtualOperation r -> VirtualT
oVirtualType Phi {}         = PhiType
oVirtualType (Delimiter o)  = DelimiterType (oDelimiterType o)
oVirtualType Kill {}        = KillType
oVirtualType Define {}      = DefineType
oVirtualType Combine {}     = CombineType
oVirtualType Low {}         = LowType
oVirtualType High {}        = HighType
oVirtualType Split2 {}      = Split2Type
oVirtualType Split4 {}      = Split4Type
oVirtualType VirtualCopy {} = VirtualCopyType
oVirtualType Fun {}         = FunType
oVirtualType (Frame o)      = FrameType (oFrameType o)

oDelimiterType :: DelimiterOperation r -> DelimiterT
oDelimiterType In {}     = InType
oDelimiterType Out {}    = OutType
oDelimiterType Entry {}  = EntryType
oDelimiterType Return {} = ReturnType
oDelimiterType Exit {}   = ExitType

oFrameType :: FrameOperation r -> FrameT
oFrameType Setup {}   = SetupType
oFrameType Destroy {} = DestroyType

-- Comparators

alternativeCopies :: Eq r => BlockOperation i r -> BlockOperation i r -> Bool
alternativeCopies o o'
    | isCopy o && isCopy o' && (oUses o == oUses o') &&
      (oDefs o == oDefs o') = True
    | otherwise = False

-- Conversion functions

fromMachineInstruction :: Read i => Read r =>
     (i -> InstructionT) ->
     (i -> ([OperandInfo rc], [OperandInfo rc])) ->
     (OperationId, MachineInstruction i r) ->
     BlockOperation i r

fromMachineInstruction itf oif (id, MachineBundle {mbInstrs = mis}) =
    let icode = map (fromMachineInstruction itf oif) (zip [id..] mis)
    in mkBundle icode

fromMachineInstruction itf oif
  (id, MachineSingle {msOpcode = opcode, msProperties = properties,
                      msOperands = operands}) =
    let (us, ds) = splitMachineOperands oif opcode operands
        us'      = map fromMachineOperand us
        ds'      = map fromMachineOperand ds
        o = case opcode of
              (MachineVirtualOpc opc) ->
                case opc of
                  PHI              -> mkPhi id us' (head ds')
                  COPY             -> mkVirtualCopy id (head us') (head ds')
                  LOW              -> mkLow id [VirtualInstruction] (head us') (head ds')
                  HIGH             -> mkHigh id [VirtualInstruction] (head us') (head ds')
                  SPLIT2           -> mkSplit2 id (head us') (ds' !! 0) (ds' !! 1)
                  SPLIT4           -> mkSplit4 id (head us') (ds' !! 0) (ds' !! 1) (ds' !! 2) (ds' !! 3)
                  IMPLICIT_DEF     -> mkDefine id [head ds']
                  ENTRY            -> mkEntry id ds'
                  RETURN           -> mkReturn id us'
                  EXIT             -> mkExit id
                  COMBINE          -> mkCombine id (us' !! 0) (us' !! 1) (head ds')
                  ADJCALLSTACKDOWN -> mkFrameSetup id (head us')
                  ADJCALLSTACKUP   -> mkFrameDestroy id (head us')
                  SUBREG_TO_REG ->
                    error ("unexpected 'SUBREG_TO_REG' machine instruction, should be lowered before this point")
              (MachineTargetOpc i) ->
                case itf i of
                  CopyInstructionType ->
                    mkCopy id [TargetInstruction i]
                    (head us') (tail us') (head ds') (tail ds')
                  LinearInstructionType ->
                    mkLinear id [TargetInstruction i] us' ds'
                  BranchInstructionType ->
                    mkBranch id [TargetInstruction i] us'
                  CallInstructionType   ->
                    mkCall id [TargetInstruction i] us'
                  TailCallInstructionType ->
                    mkTailCall id [TargetInstruction i] us'
    in o {oAs = buildInstructionAttributes properties}

fromMachineOperand :: Read r => MachineOperand r -> Operand r
fromMachineOperand MachineTemp {mtId = id} = Temporary id Nothing
fromMachineOperand MachineReg {mrName = name} = Register (mkTargetRegister name)
fromMachineOperand MachineBlockRef {mbrId = id} = BlockRef id
fromMachineOperand mo = Bound mo

splitMachineOperands
  :: (i -> ([OperandInfo rc], [OperandInfo rc])) -> MachineOpcode i ->
     [MachineOperand r] -> ([MachineOperand r], [MachineOperand r])
splitMachineOperands _ (MachineVirtualOpc opc) operands =
    splitMachineVirtualOperands opc operands
splitMachineOperands oif (MachineTargetOpc i) operands =
  let (uOpInfo, dOpInfo) = oif i
      (ds, rest)         = splitAt (length dOpInfo) operands
      (us, _)            = splitAt (length uOpInfo) rest
  in (us, ds)

splitMachineVirtualOperands opcode operands
  | opcode `elem` [ENTRY] = ([], operands)
splitMachineVirtualOperands opcode operands
  | opcode `elem` [RETURN] = (operands, [])
splitMachineVirtualOperands opcode (d:s:_)
  | opcode `elem` [COPY, LOW, HIGH] = ([s], [d])
splitMachineVirtualOperands opcode [ld, hd, u]
  | opcode `elem` [SPLIT2] = ([u], [ld, hd])
splitMachineVirtualOperands opcode [lld, lhd, hld, hhd, u]
  | opcode `elem` [SPLIT4] = ([u], [lld, lhd, hld, hhd])
splitMachineVirtualOperands opcode operands
  | opcode `elem` [IMPLICIT_DEF] = ([], operands)
splitMachineVirtualOperands opcode (operand:operands)
  | opcode `elem` [PHI, COMBINE] = (operands, [operand])
splitMachineVirtualOperands opcode (s:_)
  | opcode `elem` [ADJCALLSTACKDOWN, ADJCALLSTACKUP] = ([s], [])
splitMachineVirtualOperands opcode _
  | opcode `elem` [SUBREG_TO_REG] =
      error ("unexpected 'SUBREG_TO_REG' machine instruction, should be lowered before this point")

buildInstructionAttributes :: [MachineInstructionProperty r] -> Attributes i r
buildInstructionAttributes ips =
  let m    = fmap msPropertyMem $ find isMachineInstructionPropertyMem ips
      jtbs = map mbrId $
             fromMaybe [] $
             fmap msPropertyJTIBlocks $
             find isMachineInstructionPropertyJTIBlocks ips
      bt   = fmap msPropertyBranchTaken $
             find isMachineInstructionPropertyBranchTaken ips
  in mkNullAttributes {aMem = m, aJTBlocks = jtbs, aBranchTaken = bt}

toMachineFunction :: Show i => Show r => Function i r -> MachineFunction i r
toMachineFunction
  Function {fName = name, fCode = code, fFixedStackFrame = ffs,
            fStackFrame = fs, fJumpTable = jt, fSource = src} =
  let mff = toMachineFunctionPropertyFixedFrame ffs
      mf  = toMachineFunctionPropertyFrame fs
      mjt = toMachineFunctionPropertyJumpTable jt
      mbs = map toMachineBlock code
  in mkMachineFunction name
         (maybeToList mff ++ maybeToList mf ++ maybeToList mjt) mbs src

toMachineFunctionPropertyFixedFrame ::
    [FrameObject r] -> Maybe (MachineFunctionProperty r)
toMachineFunctionPropertyFixedFrame [] = Nothing
toMachineFunctionPropertyFixedFrame fobjs = Just $
    mkMachineFunctionPropertyFixedFrame (map toMachineFrameObjectInfo fobjs)

toMachineFunctionPropertyFrame ::
    [FrameObject r] -> Maybe (MachineFunctionProperty r)
toMachineFunctionPropertyFrame [] = Nothing
toMachineFunctionPropertyFrame fobjs = Just $
    mkMachineFunctionPropertyFrame (map toMachineFrameObjectInfo fobjs)

toMachineFrameObjectInfo :: FrameObject r -> MachineFrameObjectInfo r
toMachineFrameObjectInfo fo = mkMachineFrameObjectInfo (foIndex fo)
                              (foOffset fo) (foSize fo) (foAlignment fo)
                              (foCSRegister fo)

toMachineFunctionPropertyJumpTable ::
  (String, [JumpTableEntry]) -> Maybe (MachineFunctionProperty r)
toMachineFunctionPropertyJumpTable (_, []) = Nothing
toMachineFunctionPropertyJumpTable (k, es) =
  Just $ mkMachineFunctionPropertyJumpTable k (map toMachineJumpTableEntry es)

toMachineJumpTableEntry :: JumpTableEntry -> (MachineJumpTableEntry r)
toMachineJumpTableEntry (JumpTableEntry id bs) =
  mkMachineJumpTableEntry id (map mkMachineBlockRef bs)

toMachineBlock :: Show i => Show r => Block i r -> MachineBlock i r
toMachineBlock Block {bLab = id, bAs = as, bCode = is} =
  let mis = map toMachineInstruction is
      mps = toMachineBlockProperties as
  in mkMachineBlock id mps mis

toMachineBlockProperties :: BlockAttributes -> [MachineBlockProperty]
toMachineBlockProperties BlockAttributes {aFreq = f, aSplit = s} =
  maybeToList (fmap mkMachineBlockPropertyFreq f) ++
  [mkMachineBlockPropertySplit | s]

toMachineInstruction :: Show i => Show r => BlockOperation i r ->
                        MachineInstruction i r
toMachineInstruction Bundle {bundleOs = os} =
  let mis = map toMachineInstruction os
  in mkMachineBundle mis
toMachineInstruction o @ SingleOperation {oAs = attrs} =
  let mopc = toMachineOpcode o
      mps  = toMachineInstructionProperties attrs
      os   = oDefs o ++ oUses o
      mos  = map toMachineOperand os
  in mkMachineSingle mopc mps mos

toMachineOpcode :: Show i => BlockOperation i r -> MachineOpcode i
toMachineOpcode o
    | isPhi o = mkMachineVirtualOpc PHI
    | otherwise = mkMachineTargetOpc $ targetInst $ oInstructions o

toMachineInstructionProperties :: Attributes i r ->
                                  [MachineInstructionProperty r]
toMachineInstructionProperties Attributes {aJTBlocks = bs, aBranchTaken = bt} =
  [mkMachineInstructionPropertyJTIBlocks (map mkMachineBlockRef bs)
  | not (null bs)] ++
  [mkMachineInstructionPropertyBranchTaken (fromJust bt) | isJust bt]

toMachineOperand :: Show r => Operand r -> MachineOperand r
toMachineOperand (Register (TargetRegister r)) = mkMachineReg r
toMachineOperand (BlockRef l) = mkMachineBlockRef l
toMachineOperand (Temporary {tId = tid}) = mkMachineTemp tid [] Nothing
toMachineOperand (Bound mo) = mo

toLinear :: Operation i r -> NaturalOperation i r
toLinear Copy {oCopyIs = is,
               oCopyS = s, oCopyUs = us, oCopyD = d, oCopyDs = ds} =
  Linear {oIs = is, oUs = s:us, oDs = d:ds}

showVirtualOpc :: Operation i r -> String
showVirtualOpc o
    | isPhiOpr          o = "phi"
    | isInOpr           o = "in"
    | isOutOpr          o = "out"
    | isEntryOpr        o = "entry"
    | isReturnOpr       o = "return"
    | isExitOpr         o = "exit"
    | isKillOpr         o = "kill"
    | isDefineOpr       o = "define"
    | isCombineOpr      o = "combine"
    | isLowOpr          o = "low"
    | isHighOpr         o = "high"
    | isSplit2Opr       o = "split2"
    | isSplit4Opr       o = "split4"
    | isVirtualCopyOpr  o = "copy"
    | isFunOpr          o = "fun"
    | isFrameSetupOpr   o = "setup"
    | isFrameDestroyOpr o = "destroy"

toVirtualType :: String -> VirtualT
toVirtualType "in"      = DelimiterType InType
toVirtualType "out"     = DelimiterType OutType
toVirtualType "kill"    = KillType
toVirtualType "define"  = DefineType
toVirtualType "combine" = CombineType
toVirtualType "low"     = LowType
toVirtualType "high"    = HighType
toVirtualType "split2"  = Split2Type
toVirtualType "split4"  = Split4Type
toVirtualType "phi"     = PhiType
toVirtualType "copy"    = VirtualCopyType
toVirtualType "fun"     = FunType
toVirtualType "setup"   = FrameType SetupType
toVirtualType "destroy" = FrameType DestroyType

linearizeCode ::  [Block i r] -> [Block i r]
linearizeCode = map linearizeBlock

linearizeBlock :: Block i r -> Block i r
linearizeBlock b @ Block {bCode = bcode} =
    b {bCode = concatMap linearizeOpr bcode}

linearizeOpr :: BlockOperation i r -> [BlockOperation i r]
linearizeOpr (o @ SingleOperation {}) = [o]
linearizeOpr (o @ Bundle {}) = bundleOs o

toSubRegIndex :: Show r => MachineOperand r -> SubRegIndex
toSubRegIndex (MachineSubRegIndex sri) = NamedSubRegIndex sri
toSubRegIndex (MachineImm idx) = RawSubRegIndex idx
toSubRegIndex mop = error ("unmatched: toSubRegIndex " ++ show mop)

lowerGoal :: Read s => HighLevelGoal -> Goal s
lowerGoal Speed = DynamicGoal Cycles
lowerGoal Size  = StaticGoal (ResourceUsage (read "BundleWidth"))
lowerGoal Spill = DynamicGoal (ResourceUsage (read "SpillCost"))
