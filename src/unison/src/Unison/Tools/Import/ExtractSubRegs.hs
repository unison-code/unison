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
module Unison.Tools.Import.ExtractSubRegs (extractSubRegs) where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Tuple
import Control.Arrow

import Common.Util
import MachineIR

extractSubRegs mf _target =
  let newId       = newMachineTempId mf
      (mf',
       (_, esrs)) = traverseMachineFunction extractInstrSubRegs (newId, []) mf
      b2esr       = fromListMult $ map (first mbrId . swap) esrs
      mf''        = mf' {mfBlocks = map (appendInstrs b2esr) (mfBlocks mf')}
  in mf''

-- | Phi instructions are special: we add the extractSubReg instructions in the
-- predecessor blocks in a second pass
extractInstrSubRegs (accIs, (id, esrs))
  (mi @ MachineSingle {msOperands = (_:os)} : is)
  | isMachinePhi mi =
  let os'      = [(u, l) | [u, l] <- chunksOf 2 os]
      subtemps = nub [(mst, mbb) | (mst @ MachineSubTemp {}, mbb) <- os']
      st2t     = mapToTemps id subtemps
      esr      = [(mkExtractSubReg (mst, mt), mbb) | ((mst, mbb), mt) <- st2t]
      mi'      = replacePhiSubRegs
                 [((mst, mbb), (mt, mbb)) | ((mst, mbb), mt) <- st2t] mi
  in extractInstrSubRegs (accIs ++ [mi'], (nextId id st2t, esrs ++ esr)) is

extractInstrSubRegs (accIs, (id, esrs))
  (mi @ MachineSingle {msOperands = os} : is) =
  let subtemps = nub [mst | mst @ MachineSubTemp {} <- os]
      st2t     = mapToTemps id subtemps
      esr      = map mkExtractSubReg st2t
      mi'      = replaceSubRegs st2t mi
  in extractInstrSubRegs (accIs ++ esr ++ [mi'], (nextId id st2t, esrs)) is

extractInstrSubRegs (is, acc) [] = (is, acc)

nextId id []  = id
nextId _ st2t = maximum (map (mtId . snd) st2t) + 1

mapToTemps id subtemps = zip subtemps (map mkSimpleMachineTemp [id..])

mkSimpleMachineTemp id = mkMachineTemp id Nothing

replaceSubRegs m mi @ MachineSingle {msOperands = os} =
  mi {msOperands = map (applyMap (M.fromList m)) os}

replacePhiSubRegs m mi @ MachineSingle {msOperands = (d:os)}
  | isMachinePhi mi =
    let m'  = applyMap $ M.fromList m
        os1 = [(u, l) | [u, l] <- chunksOf 2 os]
        os2 = map m' os1
        os3 = concat [[u, l] | (u, l) <- os2]
    in mi {msOperands = d:os3}

mkExtractSubReg (MachineSubTemp {mstId = mstid, mstSubRegIndex = subreg},
                 MachineTemp    {mtId  = id, mtTiedDef = d}) =
  mkMachineSingle (mkMachineVirtualOpc EXTRACT_SUBREG) []
  [mkMachineTemp id d, mkMachineTemp mstid Nothing, mkMachineSubRegIndex subreg]

appendInstrs b2mis b @ MachineBlock {mbId = bid, mbInstructions = mis}
  | M.member bid b2mis = b {mbInstructions = mis ++ b2mis M.! bid}
  | otherwise = b
