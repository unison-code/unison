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
module Unison.Test.Invariants
    (allTemporariesDefined,
     singleDefinitions,
     allTemporariesUsed,
     allRegistersDefined,
     allRegClassesDefined,
     consistentOperandInfo,
     consistentOperands,
     consistentPreAssignments,
     noRedefinitions,
     noEdgeInterferences,
     noMustConflicts,
     noCongruentCopy,
     noIsolatedGlobals,
     uniqueOperationIds,
     uniqueOperandIds,
     singleEntryBlock,
     allEntryOpsPreAssigned,
     allExitOpsPreAssigned,
     allFunOpsPreAssigned,
     noComponentConflicts,
     noCostOverflow,
     noAmbiguousPhis,
     allResourcesDefined,
     noEmptyRegClass,
     allRegClassesReal,
     noReservedRegRedef,
     noEmptyBlock
    )
    where

import Data.List
import Data.Maybe
import Data.Function
import Data.Ord
import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.Set as S

import Common.Util

import MachineIR.Base

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Constructors
import Unison.Instances
import Unison.Target.API
import Unison.Target.Query
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.DG as DG
import Unison.Target.RegisterArray
import Unison.Analysis.TemporaryType
import Unison.Analysis.MakespanBounds

allTemporariesDefined Function {fCode = code} _ =
    testAllTemporaries temporaryDefined code

temporaryDefined code t =
  if null (filter (isPotentialDefiner t) code)
  then Just (showProblem "allTemporariesDefined"
             (show t ++ " is not defined"))
  else Nothing

singleDefinitions Function {fCode = code} _ =
    testAllTemporaries temporaryDefinedOnce code

temporaryDefinedOnce code t =
    let definers = filter (isDefiner t) code
    in if length definers > 1 then
           Just (showProblem "singleDefinitions"
                 (show t ++ " is defined more than once"))
       else Nothing

allTemporariesUsed Function {fCode = code} _ =
  testAllTemporaries temporaryUsed code

temporaryUsed code t =
    if null (filter (isPotentialUser t) code)
    then Just (showProblem "allTemporariesUsed" (show t ++ " is not used"))
    else Nothing

allRegistersDefined f target =
    let ra = mkRegisterArray target 0
    in testAllElements (definedRegisters ra) (preAssignments $ fCode f)

definedRegisters ra (_, _, r) =
    let rs = nub $ concatMap (raRegisters ra) (raRcs ra)
    in if r `elem` rs then Nothing
       else Just (showProblem "allRegistersDefined"
                  (show r ++ " is not defined in the target description"))

allRegClassesDefined f target =
  let   oif = operandInfo target
        rcs = regClasses target
  in testAllOperations (allInstrRegClassesDefined oif rcs) (fCode f)

allInstrRegClassesDefined oif rcs i
   | isVirtual i = []
   | otherwise =
     concatMap (allInstrOpRegClassesDefined oif rcs i) (oInstructions i)

allInstrOpRegClassesDefined _ _ _ (General NullInstruction) = []
allInstrOpRegClassesDefined oif rcs SingleOperation {oId = id}
                          (TargetInstruction i) =
    let (uoi, doi) = oif i
        result     = catMaybes $ map (regClassDefined rcs) (uoi ++ doi)
        header     = "in operation o" ++ show id ++
                     " (instruction " ++ show i ++ "), "
    in map (showProblem "allRegClassesDefined" . (header ++)) result

regClassDefined rcs (TemporaryInfo {oiRegClass = rc}) =
  if not (rc `elem` rcs) then
    Just ("the register class " ++ show rc ++ " is not defined in the target description")
    else Nothing
regClassDefined _ _ = Nothing

consistentOperandInfo f target =
  let oif = operandInfo target
  in testAllOperations (consistentInstrOperandInfo oif) (fCode f)

consistentInstrOperandInfo _ o | isVirtual o = []
consistentInstrOperandInfo oif o =
    concatMap (consistentOprInsOperandInfo oif o) (oInstructions o)

consistentOprInsOperandInfo _ _ (General NullInstruction) = []
consistentOprInsOperandInfo oif o @ SingleOperation {oId = id}
                          (TargetInstruction i) =
    let (uoi, doi) = oif i
        result     = consistentOperandGroupInfo (oUses o) uoi ++
                     consistentOperandGroupInfo (oDefs o) doi
        header     = "in operation o" ++ show id ++ ", "
    in map (showProblem "consistentOperandInfo" . (header ++)) result

consistentOperandGroupInfo os oi =
    if length os /= length oi then
        ["the number of operands and the number of operand info items from the target description do not match"]
    else mapMaybe consistentSingleOperandInfo (zip os oi)

consistentSingleOperandInfo (Temporary {}, TemporaryInfo {}) = Nothing
consistentSingleOperandInfo (MOperand {}, TemporaryInfo {}) = Nothing
consistentSingleOperandInfo (Bound _, BoundInfo)       = Nothing
consistentSingleOperandInfo (BlockRef _, BlockRefInfo) = Nothing
consistentSingleOperandInfo (Bound MachineNullReg, TemporaryInfo {}) = Nothing
consistentSingleOperandInfo (Bound (MachineFrameIndex {}), TemporaryInfo {}) = Nothing
consistentSingleOperandInfo (o, oi) =
    Just ("operand " ++ show o ++ " is inconsistent with the operand info " ++ show oi ++ " given in the target description")

consistentOperands f target =
  let   oif    = operandInfo target
        cg     = CG.fromFunction f
        fcode  = sortBy (comparing oId) (flatCode f)
        ra     = mkRegisterArray target 0
        rc2u   = M.fromList ([(rc, raRcUsage ra rc) | rc <- raRcs ra])
        t2w    = tempWidths ra oif fcode cg
    in testAllOperations (consistentInstrOperands oif (rc2u, t2w)) (fCode f)

consistentInstrOperands _ (_, t2w) i | isLow i || isHigh i =
  let (u, d)   = (firstTemp $ oSingleUse i, firstTemp $ oSingleDef i)
      [uw, dw] = map (width t2w) [u, d]
  in if uw /= dw * 2 then
       [showProblem "consistentOperands" "the operands of operation " ++ show i
        ++ " have inconsistent widths: " ++ show [(u, uw), (d, dw)]]
     else []
consistentInstrOperands _ (_, t2w) i | isCombine i =
  let ([u1, u2], d)  = (map firstTemp $ oUses i, firstTemp $ oSingleDef i)
      [u1w, u2w, dw] = map (width t2w) [u1, u2, d]
  in if dw /= u1w * 2 || dw /= u2w * 2 then
       [showProblem "consistentOperands" "the operands of operation " ++ show i
        ++ " have inconsistent widths: " ++ show [(u1, u1w), (u2, u2w), (d, dw)]]
     else []
consistentInstrOperands _ _ i | isVirtual i = []
consistentInstrOperands oif wInfo i =
    concatMap (consistentInstrOpOperands oif wInfo i) (oInstructions i)

consistentInstrOpOperands _ _ _ (General NullInstruction) = []
consistentInstrOpOperands oif wInfo i @ SingleOperation {oId = id}
                          (TargetInstruction op) =
    let (uoi, doi) = oif op
        result     = consistentOperandGroup (oUses i) wInfo uoi ++
                     consistentOperandGroup (oDefs i) wInfo doi
        header     = "in operation o" ++ show id ++ ", "
    in map (showProblem "consistentOperands" . (header ++)) result

consistentOperandGroup os wInfo oi =
    if length os /= length oi then
        ["the number of operands and the number of operand info items from the target description do not match"]
    else mapMaybe (consistentOperand wInfo) (zip os oi)

consistentOperand _
  (_, TemporaryInfo {oiRegClass = AbstractRegisterClass {}}) = Nothing
consistentOperand _ (Bound MachineNullReg, TemporaryInfo {}) = Nothing
consistentOperand _ (Bound (MachineFrameIndex {}), TemporaryInfo {}) = Nothing
consistentOperand (rc2u, t2w) (o, TemporaryInfo {oiRegClass = rc}) =
    if null (extractTemps o) then
       Just ("operand " ++ show o ++ " has no temporaries")
    else
      let t     = firstTemp o
          width = t2w M.! t
      in if (t2w M.! t) /= (rc2u M.! rc) then
           Just ("the inferred width of temporary " ++ show t ++ " (" ++ show width ++
                 ") is inconsistent with corresponding register class (" ++ show rc ++
                 ") given in the target description")
           else Nothing
consistentOperand _ (Bound _, BoundInfo)       = Nothing
consistentOperand _ (BlockRef _, BlockRefInfo) = Nothing
consistentOperand _ (o, oi) =
    Just ("operand " ++ show o ++ " is inconsistent with the operand info " ++ show oi ++ " given in the target description")

firstTemp = undoPreAssign . head . extractTemps
width t2w t = t2w M.! (undoPreAssign t)

consistentPreAssignments f target =
  let oif   = operandInfo target
      cg    = CG.fromFunction f
      fcode = sortBy (comparing oId) (flatCode f)
      ra    = mkRegisterArray target 0
      t2w   = tempWidths ra oif fcode cg
      r2w   = M.map snd $ inverseMap $ atomWidthToRegs ra
  in testAllOperands (consistentPreAssignment t2w r2w) (fCode f)

consistentPreAssignment t2w r2w _ t =
  case preAssignment t of
   Just r ->
     let iw = t2w M.! (undoPreAssign $ head $ extractTemps t)
         rw = r2w M.! r
     in if iw /= rw then
          Just ("the inferred width of " ++ show t ++ " (" ++ show iw ++
                ") is inconsistent with the width of its pre-assigned register "
                ++ show r ++ " (" ++ show rw ++ ")")
        else Nothing
   Nothing -> Nothing

noRedefinitions f _ = testAllOperations noIstrRedefinitions (fCode f)

noIstrRedefinitions i =
  let us = S.fromList $ tUses [i]
      ds = S.fromList $ tDefs [i]
      c  = S.toList $ us `S.intersection` ds
  in if null c then []
     else [showProblem "noIstrRedefinitions"
           ("operands " ++ show c ++ " in operation o" ++ show (oId i) ++
            " are both used and defined")]

noEdgeInterferences f _ =
    let o2p = partitionMap f
    in testAllOperations (noDelimiterInterferences o2p) (fCode f)

noDelimiterInterferences o2p i
  | isDelimiter i = noInterferences o2p (oId i) (map toCongruenceOp $ oAllOps i)
  | otherwise     = []

noInterferences o2p id os =
    let ps     = sort [(o2p M.! o, o) | o <- os, o `M.member` o2p]
        gps    = groupBy (equaling fst) ps
        groups = [map snd gp | gp <- gps, length gp > 1]
        header = "in delimiter o" ++ show id ++ ", the following operands are congruent: "
    in map (showProblem "noEdgeInterferences" . (header ++) . show) groups

noMustConflicts f @ Function {fCode = code} target =
    let apf = alignedPairs target
        ra  = mkRegisterArray target 0
        ovf = regOverlap (regAtoms ra)
        sp  = samePartitions apf f
        c1  = mapMaybe (mustConflicts (preAssignments code)) sp
        c2  = catMaybes $ concatMap (instructionMustConflicts ovf)
              (flatten code)
    in c1 ++ c2

mustConflicts must sp =
    let pms  = sort $ filter (inCongruence sp) must
        gps  = groupBy ((==) `on` mustReg) pms
    in if length gps > 1 then Just (showMustConflict pms) else Nothing

showMustConflict pms =
    let spf  = showProblem "noMustConflicts"
        gpms = groupBy ((==) `on` instrTemp) pms
        spms = filter (\pm -> length pm > 1) gpms
    in spf ("conflicting pre-assignments are defined for congruent operands: "
            ++ show pms ++
               if null spms then ""
               else ", some of them are irresolvable by copy extension: " ++ show spms)

inCongruence ps (_, t, _) = t `elem` ps

mustReg (_, _, r) = r

instrTemp (i, t, _) = (i, t)

instructionMustConflicts ovf i =
  operandMustConflicts ovf i (oUses i) ++ operandMustConflicts ovf i (oDefs i)

operandMustConflicts ovf i os =
  let os' = sortBy (comparing opReg) $ filter isPreAssigned os
      rcs = [[p1, p2] | p1 <- os', p2 <- os', p1 < p2, ovf (opReg p1) (opReg p2)]
  in map (Just . showProblem "noMustConflicts" . showOperandMustConflict i) rcs

showOperandMustConflict i rc =
  "conflicting pre-assignments in o" ++
  show (oId i) ++ ": " ++ show rc

opReg = fromJust . preAssignment

noCongruentCopy f _ =
  let o2p = partitionMap f
  in testAllOperations (noCopyInCongruentClass o2p) (fCode f)

noCopyInCongruentClass o2p o
  | isCopy o && (all (\o -> o `M.member` o2p) (oAllOps o)) =
    let (s, d) = copyOps o
        result = if o2p M.! s == o2p M.! d then
                   Just "both operands are congruent and therefore the copy cannot be scheduled"
                 else
                   Nothing
        header = "in operation o" ++ show (oId o) ++ ", "
    in map (showProblem "noCongruentCopy" . (header ++)) (maybeToList result)
  | otherwise = []

noIsolatedGlobals f _ =
  let bis  = filter isDelimiter (flatCode f)
      exBs =  exitBlocks (fCode f)
      bis' = tail bis \\ map blockOut exBs
      gos  = nub $ concatMap (map toCongruenceOp . oModelOps) bis'
      sos  = nub $ concat [[o, o'] | (o, o') <- fCongruences f]
  in testAllElements (noIsolatedGlobal sos) gos

noIsolatedGlobal sos o =
  if o `elem` sos then Nothing
  else Just (showProblem "noIsolatedGlobals"
             (show o ++ " is not present in any congruence tuple"))

uniqueOperationIds f _ =
    let idFs = M.fromListWith (+)
               (concat [[(operandId p, 1) | p <- oDefs o ++ oUseOperands o,
                         isMOperand p] | o <- flatCode f])
    in testAllElements noRepeatedOperandId (M.toList idFs)

noRepeatedOperandId (_,  1) = Nothing
noRepeatedOperandId (id, n) =
    Just (showProblem "uniqueOperandIds"
         ("there are " ++ show n ++ " operands called p" ++ show id))

uniqueOperandIds f _ =
    let idFs = M.fromListWith (+) [(oId i, 1) | i <- flatCode f]
    in testAllElements noRepeatedId (M.toList idFs)

noRepeatedId (_,  1) = Nothing
noRepeatedId (id, n) =
    Just (showProblem "uniqueOperationIds"
         ("there are " ++ show n ++ " operations called o" ++ show id))

singleEntryBlock Function {fCode = code} _ =
  case filter isEntryBlock code of
   [_] -> []
   es  -> [showProblem "singleEntryBlock"
           ("several entry blocks: " ++ show (map bLab es))]

allEntryOpsPreAssigned f _ =
    let o = blockIn $ entryBlock (fCode f)
    in testAllElements
       (preAssignedOp "allEntryOpsPreAssigned" "entry")
       (oDefs o)

allExitOpsPreAssigned f _ =
    let bs = exitBlocks (fCode f)
        os = map blockOut bs
    in testAllElements
       (preAssignedOp "allExitOpsPreAssigned" "exit")
       (concatMap oUses os)

allFunOpsPreAssigned f _ =
    let os = [o | o <- flatCode f, isFun o]
    in testAllElements
       (preAssignedOp "allFunOpsPreAssigned" "fun")
       (concatMap oAllOps os)

preAssignedOp n s o
    | isPreAssigned o = Nothing
    | otherwise =
        Just (showProblem n
             ("the " ++ s ++ " operand " ++ show o ++ " is not pre-assigned"))

noComponentConflicts f target
  | isAugmented f = []
  | otherwise  =
    let apf = alignedPairs target
        sp  = samePartitions apf f
        cg  = CG.fromFunction f
    in mapMaybe (componentConflicts cg) sp

componentConflicts cg ts =
  let compInfo = nub $ concatMap (componentInfo cg) ts
  in if length compInfo > 1 then Just (showComponentConflict ts compInfo)
                            else Nothing

componentInfo cg t =
  let n  = CG.toNodeId t
      ie = [compInfo t e | (_, _, e) <- inn cg n, isLowEdge e || isHighEdge e]
      oe = [compInfo t e | (_, _, e) <- out cg n, isCombineEdge e]
  in ie ++ oe

compInfo _ (LowEdge _)  = LowComponent
compInfo _ (HighEdge _) = HighComponent
compInfo t (CombineEdge i)
  | isCombineLowOf t i  = LowComponent
  | isCombineHighOf t i = HighComponent

data Component = LowComponent | HighComponent deriving (Eq, Ord, Show)

showComponentConflict ts compInfo =
  showProblem "noComponentConflicts"
  ("the following congruent temporaries: " ++ show ts ++
   " are defined as the following conflicting components: " ++ show compInfo)

noCostOverflow Function {fCode = code} target =
  let rm   = resourceManager target
      rwlf = readWriteLatency target
      oif  = operandInfo target
      dgs  = map (DG.fromBlock rwlf rm oif) code
      deps = map DG.dependencies dgs
  in if maxCost (rm, oif, deps) code > maxInt then
       [showProblem "noCostOverflow" "maximum cost overflow"]
     else []

noAmbiguousPhis f _ = testAllOperations noAmbiguousPhi (fCode f)

noAmbiguousPhi o
    | isPhi o =
        let ls   = map (\l -> (l, 1)) $ map snd $ phiUses o
            l2n  = combineAdd ls
            l2n' = filter (\(_, n) -> n > 1) l2n
        in map
               (\(l, n) ->
                showProblem "noAmbiguousPhis"
                "phi operation o" ++ show (oId o) ++ " has " ++ show n ++
                " uses from block b" ++ show l)
               l2n'
    | otherwise = []

allResourcesDefined f target =
  let res = S.fromList $ map resName $ resources target
  in testAllOperations (allOprResourcesDefined target res) (fCode f)

allOprResourcesDefined target res o =
  let is  = [oTargetInstr i | i <- oInstructions o, isTargetInstruction i]
  in mapMaybe (allInstrResourcesDefined target res) is

allInstrResourcesDefined target res i =
  let uses  = S.fromList $ map resource $ usages target i
      undef = S.toList $ S.difference uses res
  in if null undef then Nothing else
       Just (showProblem "allResourcesDefined"
             ("resources " ++ show undef ++ " used by instruction " ++
              show i ++ " are not defined in the target description"))

noEmptyRegClass f target =
  let rf  = registers target
      oif = operandInfo target
  in testAllOperations (noEmptyRegClassInstr rf oif) (fCode f)

noEmptyRegClassInstr _ _ i | isVirtual i = []
noEmptyRegClassInstr rf oif i =
    concatMap (noEmptyRegClassOpr rf oif) (oInstructions i)

noEmptyRegClassOpr _ _ (General NullInstruction) = []
noEmptyRegClassOpr rf oif (TargetInstruction op) =
    let (uoi, doi) = oif op
        rcs        = [rc | (TemporaryInfo {oiRegClass = rc}) <- uoi ++ doi]
    in mapMaybe (noEmptyRC rf) rcs

noEmptyRC _ InfiniteRegisterClass {} = Nothing
noEmptyRC _ AbstractRegisterClass {} = Nothing
noEmptyRC rf rc @ RegisterClass {} =
  case rf rc of
    [] -> Just (showProblem "noEmptyRegClass"
                (show rc ++ " is empty"))
    _ -> Nothing

allRegClassesReal f target =
  let oif = operandInfo target
  in testAllOperations (allOprRegClassesReal oif) (fCode f)

allOprRegClassesReal oif o
   | isVirtual o = []
   | otherwise =
     concatMap (allOprInsRegClassesReal oif o) (oInstructions o)

allOprInsRegClassesReal _ _ (General NullInstruction) = []
allOprInsRegClassesReal oif o @ SingleOperation {oId = id} (TargetInstruction i) =
    let (uoi, doi) = oif i
        result     = mapMaybe regClassReal
                     (zip (oUses o) uoi ++ zip (oDefs o) doi)
        header     = "in operation o" ++ show id ++
                     " (instruction " ++ show i ++ "), "
    in map (showProblem "allRegClassesReal" . (header ++)) result

regClassReal (op, TemporaryInfo {oiRegClass = rc})
  | isModelOperand op && isAbstractRegisterClass rc =
    Just ("operand " ++ show op ++ " is bound to " ++ show rc)
regClassReal _ = Nothing

noEmptyBlock f _ =
  let p = showProblem "noEmptyBlock"
  in map p $
     testAllElements testOneIn (fCode f) ++
     testAllElements testOneOut (fCode f)

testOneIn b @ Block {bLab = bid, bCode = code} =
  case filter isIn code of
    [o] | not (isIn $ blockIn b) ->
      Just ("operation o" ++ show (oId o) ++
            " (in) should be the first one in b" ++ show bid)
    [_] -> Nothing
    ins -> Just ("b" ++ show bid ++ " has " ++ show (length ins) ++
                 " (in) operations")

testOneOut b @ Block {bLab = bid, bCode = code} =
  case filter isOut code of
    [o] | not (isOut $ blockOut b) ->
      Just ("operation o" ++ show (oId o) ++
            " (out) should be the last one in b" ++ show bid)
    [_]  -> Nothing
    outs -> Just ("b" ++ show bid ++ " has " ++ show (length outs) ++
                  " (out) operations")

noReservedRegRedef Function {fCode = code} target =
  let rr = map (mkRegister . mkTargetRegister) $ reserved target
      eid = oId $ entryIn code
  in testAllOperations (noReservedRegRedefInOpr rr eid) code

noReservedRegRedefInOpr rr eid o
  | oId o == eid = []
  | otherwise =
      let ds = filter isModelOperand $ oDefs o
      in catMaybes $ map (noReservedRegRedefInDef rr (oId o)) ds

noReservedRegRedefInDef rr oid p =
  case preAssignment p of
   Just r | r `elem` rr ->
              let msg = "reserved register " ++ show r ++
                        " is redefined by operation o" ++ show oid
              in Just $ showProblem "noReservedRegRedef" msg
   _ -> Nothing

-- | Helper functions

samePartitions apf f =
  let sp = if isAugmented f then SG.sameOperandPartitions else SG.sameTempPartitions
  in sp (SG.fromFunction (Just apf) f)

partitionMap f =
  let pm = if isAugmented f then SG.operandPartitionMap else SG.tempPartitionMap
  in pm (SG.fromCongruences $ fCongruences f)

testAllTemporaries f code =
    let fCode  = flatten code
        ts     = tUniqueOps fCode
        result = map (f fCode) ts
    in catMaybes result

testAllOperands f code =
    let fCode  = flatten code
        ps     = concatMap oModelOps fCode
        result = map (f fCode) ps
    in catMaybes result

testAllOperations f code =
    let fCode  = flatten code
        result = concatMap f fCode
    in result

testAllElements = mapMaybe

showProblem invariantName message = invariantName ++ ": " ++ message
