{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Algorithms to infer temporary type information based on a whole function.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE PatternGuards #-}
module Unison.Analysis.TemporaryType
    (maybeTempWidth, tempWidths, widthOf, homeRegisterSpaces, classOf)
    where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Graph.Inductive
import Data.Ord

import Unison.Base
import Unison.Util
import Unison.Predicates
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.Partition as P
import Unison.Target.API
import Unison.Target.RegisterArray
import Unison.Graphs.Util
import Unison.Graphs.ThirdParty

-- | Infers the width of t via a path from some node with known width
tempWidth ra oif code cg t =
    let cs       = componentsOf cg
        cg'      = fromJust $ find (gelem (CG.toNodeId t)) cs
        cg''     = undir cg'
        tIs      = [(t, definer t code : users t code) | t <- CG.temps cg'']
        mwf      = maybeTempWidth (registerClass ra) oif (raRcUsage ra)
        (t', is) = fromJust $ find (\(t, is) -> isJust (mwf t is)) tIs
        w'       = fromJust $ mwf t' is
    in inferWidthFromPath w' cg'' t' t

-- | Infers the width of t' from the path from t (with width w) in the CG
inferWidthFromPath w cg t t' =
  case esp (CG.toNodeId t) (CG.toNodeId t') cg of
    [_] -> w -- the width of t' could be computed on itself
    p   -> foldl pathWidth w (pathToLEdges cg p)

-- | Attemps to infer the width of t from the related operations os
maybeTempWidth rcf oif uf t os =
  let rcs = mapMaybe (tempRegisterClass rcf oif t) os
      rus = map uf rcs
  in listToMaybe rus

-- | Attemps to infer the width of t from operation o
tempRegisterClass rcf _ t _ | isPreAssigned t = Just (rcf (justReg t))
tempRegisterClass rcf _ t o | (Just t') <- find (isPreAssignedAs t) (tOps [o]) =
  Just (rcf (justReg t'))
tempRegisterClass _ _ _ o | isVirtual o = Nothing
tempRegisterClass _ oif t o =
    case maybeTargetOp o of
      Nothing   -> Nothing
      (Just op) -> let t2rc = tempsToRc oif o op
                   in lookup t t2rc

isPreAssignedAs t t'
    | tId t == tId t' && isPreAssigned t' = True
    | otherwise = False

justReg = fromJust . tReg

-- | Infers the widths of all temporaries
-- | Note: this could be modeled as a constraint problem in a cleaner way
tempWidths ra oif code cg =
  let t2mw  = trivialTempWidths ra oif code cg
      ts    = [t | (t, Nothing) <- M.toList t2mw]
      cs    = componentsOf (undir cg)
      t2mw' = foldl (inferTempWidth cs) t2mw ts
      t2w   = M.map fromJust t2mw'
  in t2w

inferTempWidth cs t2mw t =
    let cg' = fromJust $ find (gelem (CG.toNodeId t)) cs
        t'  = fromJust $ find (isJust . ((M.!) t2mw)) (CG.temps cg')
        w'  = fromJust $ t2mw M.! t'
        w   = inferWidthFromPath w' cg' t' t
    in M.insert t (Just w) t2mw

trivialTempWidths ra oif code cg =
  let cgp   = P.fromGraph $ elfilter isCopyOrCongruenceEdge cg
      t2p   = SG.tempPartitionMap cgp
      p2w   = M.empty
      p2w'  = fillPreAssignedWidths ra t2p p2w code
      p2w'' = fillInstructionWidths ra oif t2p p2w' code
      t2mw  = M.map (\t -> M.lookup t p2w'') t2p
  in t2mw

isCopyOrCongruenceEdge (CopyEdge _) = True
isCopyOrCongruenceEdge (CongruenceEdge _) = True
isCopyOrCongruenceEdge _ = False

fillPreAssignedWidths ra t2p p2w code =
    let pts  = filter isPreAssigned (tOps code)
        p2w' = foldl (propagatePreAssignments ra t2p) p2w pts
    in p2w'

propagatePreAssignments ra t2p p2w t =
    let p  = t2p M.! t
        rc = lowestRc ra (fromJust $ tReg t)
        w  = raRcUsage ra rc
    in M.insert p w p2w

fillInstructionWidths ra oif t2p p2w code =
    let is   = filter hasInstruction code
        t2rc = concatMap (maybeTempsToRc oif) is
        p2w' = foldl (propagateInstruction ra t2p) p2w t2rc
    in p2w'

hasInstruction i = isNatural i || isCopy i

maybeTempsToRc oif i =
    case maybeTargetOp i of
      Nothing   -> []
      (Just op) -> tempsToRc oif i op

maybeTargetOp i =
  case filter isTargetInstruction (oInstructions i) of
    []  -> Nothing
    ops -> Just (targetInst ops)

propagateInstruction ra t2p p2w (MOperand{altTemps = ts}, rc) =
  let t2rc = [(t, rc) | t <- ts, not (isNullTemporary t)]
  in foldl (propagateInstruction ra t2p) p2w t2rc

propagateInstruction ra t2p p2w (t @ Temporary{}, rc) =
    let p = t2p M.! t
        w = raRcUsage ra rc
    in M.insert p w p2w

propagateInstruction _ _ _ (op, rc) =
  error ("unmatched: propagateInstruction " ++ show (op, rc))

-- | Computes the width of the children temporary in a CG edge based on the
-- | previous width and the edge type and direction
pathWidth w (_, _, CongruenceEdge _) = w
pathWidth w (_, _, CopyEdge _) = w
pathWidth w (p, _, LowEdge o) = lowHighWidth w p o
pathWidth w (p, _, HighEdge o) = lowHighWidth w p o
pathWidth w (p, _, SplitEdge o) = lowHighWidth w p o
pathWidth w (p, _, CombineEdge o) =
    if p `elem` useTempIds o then w * 2 else w `div` 2

lowHighWidth w p o =
  if p `elem` useTempIds o then w `div` f else w * f
  where f = widthFactor o

useTempIds = map CG.toNodeId . concatMap extractTemps . oUses

widthFactor o
  | isLow o = 2
  | isHigh o = 2
  | isSplit2 o = 2
  | isSplit4 o = 4

-- | Gives the width of temporary t, trying to infer it first from instructions
-- is and falling back to a general analysis only if needed
widthOf target ra cg f t is =
  let oif = operandInfo target
      u   = raRcUsage ra
  in case maybeTempWidth (registerClassOf target) oif u t is of
    (Just w) -> w
    Nothing  -> tempWidth ra oif (flatCode f) cg t

-- | Gives the smallest register class containing r
registerClassOf target r =
    let r'  = rTargetReg $ regId r
        rcs = [(length (registers target rc), rc)
              | rc <- regClasses target,
                isNormalRegisterClass rc,
                r' `elem` registers target rc]
    in snd $ minimumBy (comparing fst) rcs

-- | Infers the default register space of each operand (the register space to
--   which it would be allocated if no copy was active)
homeRegisterSpaces ra oif code cg =
  let p2rs  = M.fromList [(p, topIRs ra) | p <- concatMap oAllOperands code]
      t2rs  = tempHomeRegisterSpaces ra oif code cg
      p2rs' = M.mapWithKey (inferHomeFromTemp t2rs) p2rs
  in p2rs'

inferHomeFromTemp t2rs p _ = t2rs M.! (firstTemp p)

firstTemp = head . extractTemps

-- | Infers the default register space of each temporary (the register space to
--   which it would be allocated if no copy was active)
tempHomeRegisterSpaces ra oif code cg =
  let t2p    = SG.tempPartitionMap $ P.fromGraph cg
      ps     = sort $ nub $ map snd (M.toList t2p)
      p2rs   = M.fromList $ zip ps (repeat (topIRs ra))
      p2rs'  = fillPreAssignedRegRanges ra t2p p2rs code
      p2rs'' = fillInstructionRegRanges ra oif t2p p2rs' code
      t2rs   = M.map ((M.!) p2rs'') t2p
  in t2rs

fillPreAssignedRegRanges ra t2p p2rs code =
    let pts   = filter isPreAssigned (tOps code)
        p2rs' = foldl (propagatePreAssignedRegRange ra t2p) p2rs pts
    in p2rs'

propagatePreAssignedRegRange ra t2p p2rs t =
    let p  = t2p M.! (toCongruenceOp t)
        rc = lowestRc ra (fromJust $ tReg t)
        rs = raRcSpace ra rc
    in M.insert p rs p2rs

fillInstructionRegRanges ra oif t2p p2rs code =
    let is    = filter isNatural code
        op2rc = concatMap (naturalTempsToRc oif) is
        t2rc  = concat [[(t, rc) | t <- extractTemps op] | (op, rc) <- op2rc]
        p2rs' = foldl (propagateInstructionRs ra t2p) p2rs t2rc
    in p2rs'

naturalTempsToRc oif i = tempsToRc oif i (fromJust $ maybeTargetOp i)

propagateInstructionRs ra t2p p2rs (t, rc) =
    let p     = t2p M.! (toCongruenceOp t)
        rs    = raRcSpace ra rc
        newRs = lowestRsOf ra rs (p2rs M.! p)
    in M.insert p newRs p2rs

-- | Gives the class of temporary t trying to infer it from the instruction
classOf target = tempRegisterClass (registerClassOf target) (operandInfo target)

-- | Gives the lowest register class containing the given register.
lowestRc ra r =
    let fRegs = raRegisters ra
        -- Avoid fetching registers from infinite register classes at this point
        rcs   = filter (\rc -> not (isInfiniteRegisterClass rc) &&
                        r `elem` fRegs rc) (raRcs ra)
    in minimumBy (comparing (length . fRegs)) rcs

-- | Gives the lowest register space of the given two.
lowestRsOf ra rs1 rs2 =
    let rs1as = rsAtoms ra rs1
        rs2as = rsAtoms ra rs2
    in if isSubsetOf rs1as rs2as then rs1 else rs2

isSubsetOf (l1, u1) (l2, u2) = l1 >= l2 && u1 <= u2

topIRs ra = raRcSpace ra $ topRc
