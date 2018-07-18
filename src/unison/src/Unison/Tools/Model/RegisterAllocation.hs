{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Patric Hedlin <patric.hedlin@ericsson.com>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Model.RegisterAllocation (parameters) where

import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Aeson (toJSON)
import Control.Arrow
import Data.Graph.Inductive
import Unison.Graphs.ThirdParty
import Data.Tuple

import Unison
import Unison.Target.API
import Unison.Target.Query
import Unison.Target.RegisterArray
import Unison.Analysis.TemporaryType
import Unison.Analysis.CopyRelated
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.Partition as P

import Unison.Tools.Model.Definitions

parameters noCC (cg, _, _, t2w, ra, _) f @ Function {fCode = code} target =
    let oif         = operandInfo target
        bif         = branchInfo target
        apf         = alignedPairs target
        ppf         = packedPairs target
        rpf         = relatedPairs target
        ibf         = infRegClassBound target

        bcfg        = BCFG.fromFunction bif f
        fCode       = sortBy (comparing oId) (flatten code)
        im          = instructionManager fCode

        p           = codeOperands fCode
        t           = sort $ tUniqueOps fCode

        tmp         = blockMap (sort . tUniqueOps) code
        operands    = map oAllOperands fCode
        use         = toValueListM $ operandUse fCode
        p2ts        = operandTemps fCode
        temps       = toValueListM p2ts
        definer     = toValueList (tempDefiners fCode)
        operation   = toValueList (operandOperation fCode)

        sg          = SG.fromFunction (Just apf) f
        congr       = sort $ map sort $ SG.sameOperandPartitions sg

        preassign   = computeModelPreAssignments ra (preAssignments code)

        width       = toValueListM t2w
        (aligned,
         adist)     = unzip $ sort $ alignedTuples apf im t2w fCode
        packed      = packedTuples ppf fCode
        (exrelated,
         table)     = unzip $ sort $ concatMap (relatedTuples rpf ra) fCode

        pp          = map S.fromList congr
        adjacent    = sort $ BCFG.eqvNeighborTemps bcfg pp

        rs2a        = map (\rs -> (rs, rsAtoms ra rs)) $ raRss ra
        rcs         = raRcs ra
        atoms       = map (rcAtoms ra) rcs
        ocf         = operationClass oif ra im
        rclass      = map ocf fCode

        rs          = map fst rs2a

        copyrel     = concatMap copyRelatedOperands code

        classSpace  = M.fromList
                      [(raIndexedRc ra rc, raRcSpace ra rc) | rc <- raRcs ra]
        infinite    = map rsInfinite rs
        bounded     = map (isBoundedSpace ibf classSpace) rs

        atomname    = toValueListM $ atomToAtomicRegName ra

        classname   = map (show . rcClass . raIndexedRc ra) rcs
        spacename   = map rsName rs

        r2as        = regAtoms ra
        toAtoms     = concatMap ((M.!) r2as) .
                      map (mkRegister . mkTargetRegister)
        callersaved = if noCC then [] else toAtoms $ callerSaved target
        calleesaved = if noCC then [] else toAtoms $ calleeSaved target

        space       = toValueListM classSpace
        range       = toValueList rs2a

        home        = toValueListM $ homeRegisterSpaces ra oif fCode cg
        cfg         = edges bcfg
        clusters    = computeRematClusters code adjacent
        def_opr'    = tempOprDefiners fCode
        def_opr     = toValueListM def_opr'
        allClasses  = nub $ concat $ concat rclass
        infassign   = concat [infAssignForSpace ocf classSpace
                              t def_opr' (M.fromList rs2a) t2w p2ts congr
                              packed rs'
                             | rs' <- rs,
                               rsInfinite rs',
                               none (isBounded ibf classSpace rs') allClasses]
    in
     [
      -- Program parameters

      -- set of operands
      ("P", toJSON p),

      -- set of temporaries
      ("T", toJSON t),

      -- operands of each instruction
      -- example: operands[6][2]: third operand of i6
      ("operands", toJSON operands),

      -- temporaries that can implement each operand
      ("temps", toJSON temps),

      -- whether each operand is a use
      -- example: use[5]: whether p5 is a use operand
      ("use", toJSON use),

      -- adjacent operands
      -- example: adjacent[3][0]: predecessor in the fourth operand adjacency
      --          adjacent[3][1]: successor in the fourth operand adjaceny
      ("adjacent", toJSON adjacent),

      -- pre-assignments from operands to registers
      -- example: preassigns[2][0]: operand in the third pre-assignment
      --          preassigns[2][1]: register in the third pre-assignment
      ("preassign", toJSON preassign),

      -- number of register atoms that each temporary occupies
      -- example: width[6]: number of register atoms that t6 occupies
      ("width", toJSON width),

      -- Processor parameters

      -- atoms of each register class
      -- example: atoms[4][8]: eight atom of the register class rc4
      ("atoms", toJSON atoms),

      -- register class in which each operation implemented by each
      --   instruction accesses its operands
      -- example: class[14][0][2]: register class in which o14 accesses its
      -- third operand when implemented by its first instruction in
      -- "instructions"
      ("class", toJSON rclass),

      -- aligned operand tuples
      -- example: aligned[5][0]: first operand in the sixth aligned tuple
      --          aligned[5][1]: instruction related to the first operand
      --          aligned[5][2]: second operand in the sixth aligned tuple
      --          aligned[5][3]: instruction related to the second operand
      ("aligned", toJSON aligned),

      -- alignment distance of each aligned operand tuple
      -- example: adist[5]: alignment distance of the sixth aligned tuple
      ("adist", toJSON adist),

      -- packed operand pairs
      -- example: pack[3][0]: bound operand in the fourth packed pair
      --          pack[3][1]: free operand in the fourth packed pair
      ("packed", toJSON packed),

      -- operands related extensionally
      -- example: erelated[5][0]: first operand in the sixth ext. related pair
      --          erelated[5][1]: second operand in the sixth ext. related pair
      ("exrelated", toJSON exrelated),

      -- table of register assignments of each related operand pair
      -- example: table[5][2][1]: register of second operand in the third row
      ("table", toJSON table),

      -- Additional parameters

      -- congruent operand classes
      -- example: congr[6][3]: fourth operand in the seventh congruence class
      ("congr", toJSON congr),

      -- operand that potentially defines each temporary
      -- example: definer[7]: operand that defines t7
      ("definer", toJSON definer),

      -- operation that contains each operand
      -- example: operation[3]: operation that contains p3
      ("operation", toJSON operation),

      -- temporaries defined and used within each block
      -- example: tmp[8][3]: fourth temporary of b8
      ("tmp", toJSON tmp),

      -- set of register spaces
      ("RS", toJSON rs),

      -- copy-related operand classes
      -- example: copyrel[4][1]: second operand in the fifth copy-related class
      ("copyrel", toJSON copyrel),

      -- register space of each register class
      -- example: space[4]: register space of the register class rc4
      ("space", toJSON space),

      -- atom range of each register space
      -- example: range[3][0]: first atom of the register space rs3
      -- example: range[3][1]: last atom of the register space rs3
      ("range", toJSON range),

      -- example: home[21]: home register space of p21
      ("home", toJSON home),

      -- whether each register space is infinite
      -- example: infinite[3]: whether register space rs3 is infinite
      ("infinite", toJSON infinite),

      -- whether each register space is bounded
      -- example: bounded[2]: whether register space rs2 is bounded
      ("bounded", toJSON bounded),

      -- name of each atom
      -- example: atomname[5]: name of the atom a5
      ("atomname", toJSON atomname),

      -- name of each register class
      -- example: classname[3]: name of the register class rc3
      ("classname", toJSON classname),

      -- name of each register space
      -- example: spacename[2]: name of the register space rs2
      ("spacename", toJSON spacename),

      -- caller-saved register atoms
      -- example: callersaved[3]: fourth caller-saved atom
      ("callersaved", toJSON callersaved),

      -- callee-saved register atoms
      -- example: calleesaved[1]: second callee-saved atom
      ("calleesaved", toJSON calleesaved),

      -- control edges in the block control-flow graph
      -- example: cfg[5][0]: parent of the sixth edge in the control-flow graph
      --          cfg[5][1]: child of the sixth edge in the control-flow graph
      ("cfg", toJSON cfg),

      -- clusters of global operands that must be connected simultaneously
      -- example: clusters[0][2]: third operand in the first cluster
      ("clusters", toJSON clusters),

      -- operation that potentially defines each temporary
      -- example: def_opr[6]: operation that defines temporary t6
      ("def_opr", toJSON def_opr),

      -- register atoms for temporaries assigned to infinite spaces
      -- example: infassign[2][0]: temporary of the third assignment
      -- example: infassign[2][1]: infinite register space of the third assignment
      -- example: infassign[2][2]: first register atom of the third assignment
      -- example: infassign[2][3]: last register atom of the third assignment
      ("infassign", toJSON infassign)

     ]

operandUse :: Ord r => [BlockOperation i r] -> M.Map (Operand r) Bool
operandUse code = M.fromList $ concatMap operandUseInInstr code

operandUseInInstr i =
  [(toCongruenceOp p, True) | p <- oUseOperands i] ++
  [(toCongruenceOp p, False) | p <- oDefOperands i]

operandTemps code = M.fromList $ concatMap operandTempsInInstr code

operandTempsInInstr i =
  [(id, c) | MOperand {operandId = id, altTemps = c} <- oAllOperands i]

tempDefiners code =
    [(fromSingleton (extractTemps p), p) | p <- concatMap oDefOperands code]

tempOprDefiners code =
  M.fromList [(t, nullDefiner t code) | t <- tUniqueOps code]

nullDefiner t = fromJust . find (isNullDefiner t)

isNullDefiner t i = any (isNullEquivalentTo t) (oDefs i)

isNullEquivalentTo t t' = isEquivalentTo (cleanNullTemps t) (cleanNullTemps t')

cleanNullTemps tc @ MOperand {altTemps = ts} =
    tc {altTemps = filter (not . isNullTemporary) ts}
cleanNullTemps t = t

operandOperation code =
    concat [[(p, o) | p <- oAllOperands o] | o <- code]

computeModelPreAssignments ra pas =
  let r2a = regFirstAtom ra
  in map (second ((M.!) r2a . fst)) (combinePreAssignments pas)

alignedTuples apf im t2w =
  let iif = toIndexedInstruction im
  in sort . concatMap (oprAlignedTuples apf iif t2w)

oprAlignedTuples apf iif t2w o
  | isLow o  = [((oSingleDef o, iif i, oSingleUse o, iif i), 0)
               | i <- oInstructions o, not (isNullInstruction i)]
  | isHigh o =
    let u = oSingleUse o
    in [((oSingleDef o, iif i, u, iif i), operandWidth t2w u `div` 2)
       | i <- oInstructions o, not (isNullInstruction i)]
  | isSplit2 o  =
    let u = oSingleUse o
        w = operandWidth t2w u `div` 2
    in concat [[((oDefs o !! fromInteger idx, iif i, u, iif i), w * idx)
               | idx <- [0..1]]
              | i <- oInstructions o, not (isNullInstruction i)]
  | isSplit4 o  =
    let u = oSingleUse o
        w = operandWidth t2w u `div` 4
    in concat [[((oDefs o !! fromInteger idx, iif i, u, iif i), w * idx)
               | idx <- [0..3]]
              | i <- oInstructions o, not (isNullInstruction i)]
  | isCombine o =
    let d = oSingleDef o
    in concat
       [[((head $ oUses o, iif i, d, iif i), 0),
         ((last $ oUses o, iif i, d, iif i), operandWidth t2w d `div` 2)]
        | i <- oInstructions o, not (isNullInstruction i)]
  | otherwise = [((p, iif i, q, iif i), 0)
                | (p, q, i) <- apf o]

packedTuples ppf fcode = sort $ concatMap ppf fcode

relatedTuples rpf ra o =
  let r2a    = regFirstAtom ra
      row2as = mapTuple (\p -> r2a M.! mkRegister (mkTargetRegister p))
  in [((p, q), map row2as table) | (RegisterTable p q table) <- rpf o]

operationClass oif ra om o =
    map (instructionClass oif ra o) (oIInstructions om o)

instructionClass _ ra o instr
  | ioInstruction instr `elem` [General NullInstruction,
                                General VirtualInstruction,
                                General BarrierInstruction] =
    topClasses ra o
instructionClass oif ra o (IndexedInstruction _ (TargetInstruction instr)) =
    let t2rc  = tempsToIRc ra oif o instr
        t2src = M.toList $ M.fromListWith (lowestRc ra) t2rc
    in map snd t2src

tempsToIRc ra oif o op = map (second (raIndexedRc ra)) $ tempsToRc oif o op

lowestRc ra rc1 rc2 =
  let rc2as = M.fromList [(rc, rcAtoms ra rc) | rc <- raRcs ra]
      as2rc = inverseMap rc2as
      aset  = (M.!) rc2as . rcClass
      as    = intersect (aset rc1) (aset rc2)
      lrc   = raIndexedRc ra (as2rc M.! as)
  in lrc

topClass ra = raIndexedRc ra topRc

topClasses :: Eq r => RegisterArray r rc -> BlockOperation i r ->
              [IndexedRegisterClass rc]
topClasses ra o = replicate (choices o) (topClass ra)

choices :: Eq r => BlockOperation i r -> Int
choices = length . oAllOperands

atomToAtomicRegName ra =
  let r2rc  = M.fromList $
              concat [[(r, rc) | r <- raRegisters ra rc] | rc <- raRcs ra]
      r2a   = M.toList $ regFirstAtom ra
      a2rs  = [(a, [(raRcUsage ra (r2rc M.! r), r)]) | (r, a) <- r2a]
      a2rss = M.toList $ M.fromListWith (++) a2rs
      a2rs' = M.fromList $ map (second (snd . minimum)) a2rss
  in a2rs'

operandWidth t2w p = t2w M.! (head $ extractTemps p)

codeOperands :: Eq r => [BlockOperation i r] -> [Operand r]
codeOperands = concatMap oAllOperands

-- | Gives a map from registers to their corresponding first atoms
regFirstAtom ra = M.map head (regAtoms ra)

type OpId = Int

computeRematClusters ::
    Ord r => Show r => [Block i r] -> [(Operand r, Operand r)] -> [[Operand r]]
computeRematClusters code adj =
    let fcode    = flatten code
        nullable = S.fromList $ map toCongruenceOp $
                   filter isNullableOperand $ codeOperands fcode
        adj'     = filter
                   (\(p, q) -> S.member p nullable || S.member q nullable) adj
        succ     = M.toList $ fromListMult adj'
        pr1      = P.fromPairList [(p, q) | (p, [q]) <- succ]
        pr2      = P.complete pr1 $ nub $ concat [[p, q] | (p, q) <- adj']
        use      = S.fromList [opId p | (p, True) <- M.toList $ operandUse fcode]
        block    = M.fromList $ concat $
                   map (\b -> [(opId p, bLab b)
                              | p <- codeOperands (bCode b)]) code
        atemps   = (M.fromList
                    [(opId p,
                     S.fromList (filter (not . isNullTemporary) (altTemps p)))
                     | p <- codeOperands fcode])
        mandef   = S.fromList $
                   map mkTemp $ defTemporaries $ filter (not . isCopy) fcode
        pr3      = fixpoint (joinClusters (use, block, atemps, mandef)) pr2
        pr4      = fixpoint (joinClusterCycles adj') pr3
        rcs      = sort $ map (sort . map toOperandRef) $ P.toList pr4
    in rcs

opId (MOperand {operandId = id}) = fromInteger id
opId (OperandRef {operandRefId = id}) = fromInteger id

joinClusters :: Ord r =>
                (S.Set OpId, M.Map OpId BlockId,
                 M.Map OpId (S.Set (Operand r)), S.Set (Operand r)) ->
                Partition OpId -> Partition OpId
joinClusters (use, block, atemps, mandef) pr =
  let joinables = filter (joinable (atemps, mandef)) $
                  concat [[(p, q) |
                           p <- rc1, q <- rc2,
                           block M.! p == block M.! q,
                           S.notMember p use,
                           S.member q use]
                         | rc1 <- P.toList pr, rc2 <- P.toList pr, rc1 /= rc2]
      in foldl P.connectElements pr joinables

toOperandRef = mkOperandRef . toInteger

joinable (atemps, mandef) (p, q) =
    let pts = atemps M.! p
        qts = atemps M.! q
    in S.isSubsetOf qts mandef && not (S.null (S.intersection pts qts))

joinClusterCycles adj pr =
    let toId   = fromIntegral . operandRefId
        p2gc   = M.fromList $ concat $
                 [[(p, gc) | p <- ps] | (ps, gc) <- zip (P.toList pr) [0..]]
        gc2p   = M.fromList $ [(gc, p) | (p, gc) <- M.toList p2gc]
        nodes  = [(gc, ()) | gc <- nub $ sort $ M.elems p2gc]
        edges  = [(p2gc M.! toId q, p2gc M.! toId p, ()) | (p, q) <- adj]
        edges' = nub $ sort $ filter (\(p, q, ()) -> p /= q) edges
        g      = mkGraph nodes edges' :: Gr () ()
        cycles = [[gc2p M.! p | p <- ps] | ps <- cyclesIn' g]
        conn   = concat [[(p, q) | p <- ps, q <- ps, p < q] | ps <- cycles]
        pr'    = foldl P.connectElements pr conn
    in pr'

isBounded ibf classSpace rs irc =
  let rc = rcClass irc
  in classSpace M.! irc == rs && isJust (ibf rc)

isBoundedSpace ibf classSpace rs
  | rsInfinite rs =
    let ircs = (fromListMult $ map swap (M.toList classSpace)) M.! rs
    in any (isJust . ibf . rcClass) ircs
  | otherwise = False

infAssignForSpace ocf rcSpace ts def_opr rs2a t2w p2ts congr packed rs =
    let pts     = S.fromList $ concatMap (extractTemps . snd) packed
        cts1    = map (map mkTemp) $ P.toList $
                  foldl (mergeCongruences p2ts) (P.fromNodes ts) congr
        -- cts3 contains sets of temporaries representing the same original
        -- temporary and can thus be assigned the same infinite space atoms
        cts2    = cfilter (isSpaceCandidate ocf rcSpace def_opr rs) cts1
        cts3    = map sort cts2
        cts4    = filter (\ts -> none (\t -> S.member t pts) ts) cts3
        -- this ensures a correct register bank alignment
        cts5    = sortBy (comparing (\ts -> - t2w M.! head ts)) cts4
        fa      = fst $ rs2a M.! rs
        (_, ia) = mapAccumL (buildInfAssign t2w) fa cts5
        ia'     = sort $
                  concat [map (\t -> (t, rs, a, la)) ts | (ts, (a, la)) <- ia]
    in ia'

cfilter :: (a -> Bool) -> [[a]] -> [[a]]
cfilter f cts = filter (not . null) $ map (filter f) cts

mergeCongruences p2ts cts ps =
  let rtf = not . isNullTemporary
      ts  = nub $ concat [filter rtf (p2ts M.! operandRefId p) | p <- ps]
  in foldl P.connectElements cts (pairs ts)

isSpaceCandidate ocf rcSpace def_opr rs t =
    let ps   = zip [0..] (oAllOperands $ def_opr M.! t)
        tidx = fst $ fromJust $ find (\(_, p) -> t `elem` altTemps p) ps
        rcs  = [ircs !! tidx | ircs <- ocf $ def_opr M.! t]
    in any (\rc -> rs == (rcSpace M.! rc)) rcs

buildInfAssign t2w a ts =
  let w = fromInteger (t2w M.! head ts)
      n = fromInteger (toInteger (length ts))
  in (a + (w * n), (ts, (a, a + w * (n - 1))))
