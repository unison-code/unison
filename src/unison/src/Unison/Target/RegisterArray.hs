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
module Unison.Target.RegisterArray
    (topRc,
     tempsToRc,
     atomWidthToRegs,
     atomToMatchingRegs,
     atomsToRegs,
     infRegPlace,
     registerClass,
     regOverlap,
     maxTempWidth,
     mkRegisterArray)
    where

import Data.List
import Data.Ord
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Common.Util

import Unison.Base hiding (toNode)
import Unison.Predicates
import Unison.Constructors
import Unison.Util
import Unison.Instances
import Unison.Target.API
import Unison.Target.Query

data RegSpace = RegSpace RegisterSpaceName deriving (Eq, Show, Ord)

-- | Computes a finite set of registers to represent an infinite register class
mkInfiniteRegisters (rcuf, rcbf) (Just inf) rc @ (InfiniteRegisterClass trc) =
    let inf'     = case rcbf rc of
                     Nothing -> inf
                     (Just bound) -> min bound inf
        pre      = map toLower $ takeWhile (not . isDigit) $ show trc
        u        = rcuf rc
        regRanges = [(RegisterAtom i, RegisterAtom (i + u - 1))
                         | i <- [0, u..(inf' - 1)]]
    in map (mkRegister . mkInfiniteRegister pre) regRanges

-- | Given an infinite register, returns the index of its first register atom
infRegPlace = raId . fst . rInfRegRange

-- | Given a register name, returns its register class
registerClass ra r =
  fromJust $ find (\rc -> r `elem` (raRegisters ra rc)) (raRcs ra)

-- | Gives the top register class.
topRc = TopRegisterClass

-- | Transforms a register class into an indexed register class according to
-- | the given map.
toIndexedRegisterClass rc2id rc = IndexedRegisterClass (rc2id M.! rc) rc

-- | Gives an association list with temporaries in the instruction and register
-- | classes corresponding to the given operation.
tempsToRc oif i op =
  nub $ sort $ [(op, rc) | (op, TemporaryInfo {oiRegClass = rc})
                           <- tempsToInfo oif i op,
                           isDefinedRegisterClass rc, isModelOperand op]

-- | Gives a map from (atom, width) to their corresponding register
atomWidthToRegs ra =
    M.fromList [((head a, toInteger $ length a), r)
                    | (r, a) <- M.toList (regAtoms ra)]

-- | Gives the smallest set of registers that covers the given atoms ras
atomsToRegs a2mrs ras = matchRegs a2mrs [] (sort ras)

matchRegs _ rs [] = rs
matchRegs r2as rs ras =
  let leaderRa    = head ras
      atomGroup   = r2as M.! leaderRa
      matchesWith = flip isPrefixOf
      (Just ra)   = find (matchesWith ras . fst) atomGroup
  in matchRegs r2as (rs ++ [snd ra]) (drop (length (fst ra)) ras)

-- | Gives a map from atoms to their corresponding registers
atomToMatchingRegs r2as =
  let as2r  = sort [(head as, (as, r)) | (r, as) <- M.toList r2as]
      as2rs = groupBy (equaling (raId . fst)) as2r
      a2mrs = M.fromList $ map combineAtomGroup as2rs
  in a2mrs

combineAtomGroup group =
  let leaderRa = fst (head group)
      ra2rs    = map snd group
  in (leaderRa, sortBy (comparing (negate . length . fst)) ra2rs)

-- | Computes an upper bound on the register file width that temporaries can
-- | occupy on any cycle.
maxTempWidth tight code t2w
  | tight     = roundToRc (maximum $ map (maxTWInCode t2w . bCode) code)
  | otherwise = roundToRc (maxTWInCode t2w (flatten code))

maxTWInCode t2w code = sum $ map ((M.!) t2w) (tUniqueOps code)

-- TODO: round up so that the result is a multiple of the width of the infinite
-- register space for which the bound is computed (hard-coded to 32 here)
roundToRc n =
    let n' = toRational n
        w  = 32
    in w * (ceiling (n' / (toRational w)))

-- | Tests whether r1 and r2 overlap according to a map from registers to atoms
regOverlap r2as (Register r1) (Register r2) =
  let asR1 = S.fromList $ r2as M.! (Register r1)
      asR2 = S.fromList $ r2as M.! (Register r2)
  in not $ S.null $ S.intersection asR1 asR2

-- | Builds a representation of the register array, including query
-- functions. Information about the register file should be accessed through
-- this interface.
mkRegisterArray target inf =
    let rcf  = [topRc] ++
               filter (not . isAbstractRegisterClass) (regClasses target)
        ra   = registerArray target
        rf   = map (mkRegister . mkTargetRegister) . registers target
        rcuf = infRegClassUsage target
        rcbf = infRegClassBound target
        raf  = registerAtoms target
        rf'  = expandRegClass ([], rf, rcuf, rcbf, inf)
        era  = concatMap rf' ra
        rf'' = expandRegClass (era, rf', rcuf, rcbf, inf)
        rc2u = M.fromList [(rc, classUsage rcuf rf'' r2a' rc) | rc <- rcf]
        rcId = M.fromList (zip rcf [0..])
        r2a  = M.fromList (zip era (map toSingleton [RegisterAtom 0..]))
        raf' = expandRegisterAtoms raf
        r2a' = M.union r2a
               (M.fromList $ concatMap
                (\rc -> [(r, wRegAtoms raf' r2a r) | r <- rf'' rc]) rcf)
        rcAs = \rc -> concatMap ((M.!) r2a') (rf'' rc)
        rcAf = \rc -> map (head . (M.!) r2a') (rf'' rc)
        rsi1 = [(rc, regClassSpaceInfo rcAs rc) | rc <- rcf]
        rsi2 = nub (snd $ unzip rsi1)
        rsi3 = map (\(i, (RegSpace rs, as, inf)) ->
                         (RegisterSpace i rs inf, as)) (zip [0..] rsi2)
        rss  = fst $ unzip rsi3
    in
     RegisterArray {
        raRcs          = rcf,
        raRegisters    = rf'',
        raRcUsage      = (M.!) rc2u,
        raIndexedRc    = toIndexedRegisterClass rcId,
        regArray       = era,
        regAtoms       = r2a',
        rcAtoms        = rcAf,
        raRss          = rss,
        rsAtoms        = (M.!) (M.fromList rsi3),
        raRcSpace      = classRegisterSpace (regClassSpaceInfo rcAs) rss
      }

expandRegClass (era, _, _, _, _) TopRegisterClass = era
expandRegClass (_, rf, _, _, _) rc @ RegisterClass {} = rf rc
expandRegClass (_, _, rcuf, rcbf, inf) rc @ InfiniteRegisterClass {} =
    mkInfiniteRegisters (rcuf, rcbf) (Just inf) rc
expandRegClass _  AbstractRegisterClass {} = []

classUsage rcuf rf r2a rc
    | isInfiniteRegisterClass rc = rcuf rc
    | otherwise = toInteger $ length $ r2a M.! (head (rf rc))

expandRegisterAtoms _ (Register (InfiniteRegister pre (fa, la))) =
    (mkRegister $ mkInfiniteRegister pre (fa, fa),
     mkRegister $ mkInfiniteRegister pre (la, la))
expandRegisterAtoms raf (Register (TargetRegister r)) =
    mapTuple (mkRegister . mkTargetRegister) (raf r)

wRegAtoms raf r2a r =
    let (fs, ls) = mapTuple ((M.!) r2a) (raf r)
    in [minimum fs..maximum ls]

regClassSpaceInfo rcAs rc =
    case rcAs rc of
      [] -> (RegSpace "empty", (RegisterAtom 0, RegisterAtom 0), False)
      as ->
          let (f, l) = (minimum as, maximum as)
              rs     = RegSpace (show f ++ "-" ++ show l)
          in (rs, (f, l), isInfiniteRegisterClass rc)

classRegisterSpace rcsi irss rc =
    let (rs, _, _) = rcsi rc
    in toRegisterSpace irss rs

toRegisterSpace irss (RegSpace name) =
    fromJust $ find (\irs -> rsName irs == name) irss
