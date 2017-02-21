{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Analyze.InsertNops (insertNops) where

import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Unison
import Unison.Target.API
import Unison.Target.Query
import Unison.Target.RegisterArray
import Unison.Analysis.Stalls

insertNops f @ Function {fCode = code} target =
    let oif     = operandInfo target . targetInst . oInstructions
        nf      = nop target
        ra      = mkRegisterArray target 0
        uf      = usages target
        cf      = capacityMap target
        ovf     = safeRegOverlap (regAtoms ra)
        arf     = allRegisters ra
        id      = newId (linearizeCode code)
        (_,
         code') = mapAccumL
                  (insertBlockNops (oif, ovf, uf, cf, nf, arf)) id code
    in f {fCode = code'}

insertBlockNops fs id b @ Block {bCode = code} =
    let blocked = S.empty :: S.Set (BlockingResourceState r s)
        ((_, id'),
         codes) = mapAccumL (insertNopsBefore fs) (blocked, id) code
        code'   = concat codes
    in (id', b {bCode = code'})

insertNopsBefore fs (blocked, id) o
    | not (isBundle o) = insertNopsBefore fs (blocked, id) (mkBundle [o])

insertNopsBefore fs @ (oif, ovf, uf, cf, nf, arf) (blocked, id)
  o @ Bundle {bundleOs = bos} =
    let -- Update blocked list after new cycle
        blocked'   = S.filter isActive $ S.map stepCycle blocked
        -- Compute whether there are stalls due to uses of blocked registers
        us         = concatMap (uses arf oif) bos
        stalls     = not $ null $ catMaybes $ map (longestStall ovf blocked') us
        -- Compute whether there are stalls due to exceeding resource capacities
        bUsages    = S.toList $ S.filter isBlockingRes blocked'
        newUsages  = concatMap (resUsages uf cf) bos
        usages     = combineAdd $ map toMapTuple $
                     bUsages ++ filter isBlockingRes newUsages
        exceeds    = any (exceedsCapacity cf) usages
    in if exceeds && null bUsages then
           error ("the following bundle exceeds resource capacity: " ++ show o)
       else
           if stalls || exceeds then
               -- add nop and recurse
               let ((blocked''', id'),
                    os) = insertNopsBefore fs (blocked', id) o
                   os'  = [mkSingleOperation id' nf] ++ os
               in ((blocked''', id' + 1), os')
           else -- proceed to next bundle
               let
                   -- Update blocked list with the definitions in o
                   ds         = concatMap (defs arf oif) bos
                   blocked''  = foldl (flip S.insert) blocked' ds
                   -- Update blocked list with the resources consumed by o
                   blocked4   = foldl (flip S.insert) blocked'' newUsages
               in ((blocked4, id), [o])

uses _ _ o | isVirtual o = []
uses arf oif o = [BlockingResourceState (BlockingReg r) l 0
                      | (TemporaryInfo {oiLatency =l}, r)
                        <- zip (fst $ oif o) (oUses o), S.member r arf]
defs _ _ o | isVirtual o = []
defs arf oif o = [BlockingResourceState (BlockingReg r) l 0
                      | (TemporaryInfo {oiLatency = l}, r)
                        <- zip (snd $ oif o) (oDefs o), S.member r arf]

isBlockingRes (BlockingResourceState BlockingRes {} _ 0) = True
isBlockingRes _ = False

longestStall ovf blocked (BlockingResourceState r l _) =
    let alias  = filter (aliasesWith ovf r) (S.toList blocked)
        alias' = map (incrementOccupationBy l) alias
    in case alias' of
         [] -> Nothing
         as -> case maximumBy (comparing brsOccupation) as of
                 BlockingResourceState {brsOccupation = s} | s > 0 -> Just s
                 _ -> Nothing

incrementOccupationBy l brs @ BlockingResourceState {brsOccupation = occ} =
  brs {brsOccupation = occ + l}

aliasesWith ovf (BlockingReg r)
  BlockingResourceState {brsResource = BlockingReg r'} = ovf r r'
aliasesWith _ _ _ = False

safeRegOverlap r2as r r'
    | M.member r r2as && M.member r r2as = regOverlap r2as r r'
    | otherwise = False

-- | Gives the set of defined registers in the processor
allRegisters ra = S.fromList $ concatMap (raRegisters ra) (raRcs ra)
