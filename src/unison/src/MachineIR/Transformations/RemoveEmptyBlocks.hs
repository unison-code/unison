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
module MachineIR.Transformations.RemoveEmptyBlocks
    (removeEmptyBlocks) where

import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

import Common.Util
import MachineIR

removeEmptyBlocks mf @ MachineFunction {mfBlocks = code} _target =
    let ids    = map mbId code
        -- Blocks which are referred by phi instructions are forbidden as
        -- removing them might render the phi instructions ambiguous.
        -- TODO: this is very conservative, do it only if some phi instruction
        -- becomes ambiguous for sure!
        forbid = S.fromList $
                 concatMap
                 (\mb -> concatMap phiLabels (mbInstructions mb)) code
        smap   = M.fromList [(id, id) | id <- ids]
        ((_, pmap', smap', removedFreqs), mcode) =
                 mapAccumL (removeEmptyBlock forbid) (ids, smap, smap, S.empty)
                 code
        mf1    = mf {mfBlocks = catMaybes mcode}
        isPhi  = isMachinePhi
        mf2    = mapToMachineBlockId (not . isPhi) (applyMap smap') mf1
        mf3    = mapToMachineBlockId isPhi (applyMap pmap') mf2
        mf4    = mf3 {mfProperties =
                      addRemovedFreqs (mfProperties mf3) removedFreqs}
    in mf4

removeEmptyBlock forbid (ids, pmap, smap, removedFreqs)
  mb @ MachineBlock {mbId = id, mbInstructions = []}
      | S.notMember id forbid =
    let nid   = next id ids
        pid   = previous id ids
        ids'  = delete id ids
        pmap' = M.insert id pid pmap
        smap' = M.insert id nid smap
        removedFreqs' = S.insert (machineBlockFreq mb) removedFreqs
    in ((ids', pmap', smap', removedFreqs'), Nothing)
removeEmptyBlock _ acc mb = (acc, Just mb)

next     e l = l !! (p + 1) where p = fromJust $ elemIndex e l
previous e l = l !! (p - 1) where p = fromJust $ elemIndex e l

phiLabels mi @ MachineSingle {msOperands = mos}
  | isMachinePhi mi = map mbrId $ filter isMachineBlockRef mos
  | otherwise = []
phiLabels MachineBundle {mbInstrs = mis} = concatMap phiLabels mis

addRemovedFreqs mps rfs
  | any isMachineFunctionPropertyRemovedFreqs mps =
      map (addRemovedFreqsToProperty rfs) mps
  | otherwise =
      mps ++ [mkMachineFunctionPropertyRemovedFreqs (S.toList rfs)]

addRemovedFreqsToProperty rfs (MachineFunctionPropertyRemovedFreqs fs) =
  let fs' = S.toList $ S.union (S.fromList fs) rfs
  in mkMachineFunctionPropertyRemovedFreqs fs'
addRemovedFreqsToProperty _ mp = mp
