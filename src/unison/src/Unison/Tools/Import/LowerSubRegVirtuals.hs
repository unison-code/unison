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
module Unison.Tools.Import.LowerSubRegVirtuals (lowerSubRegVirtuals) where

import MachineIR
import Unison
import Unison.Target.API

import Data.List.Split
import Control.Arrow
import qualified Data.Map as M

lowerSubRegVirtuals mf @ MachineFunction {} target =
  let stf      = subRegIndexType target
      tid2rc   = registerClassMap mf
      newId    = newMachineTempId mf
      (mf', _) = traverseMachineFunction (lowerSubRegVirtual stf tid2rc)
                 newId mf
  in mf'

lowerSubRegVirtual stf tid2rc (accIs, id) (mi @
  MachineSingle {msOperands = [d @ MachineTemp {},
                               s @ MachineTemp {mtId = sid}, sr]} : mis)
  | isMachineExtractSubReg mi =
    let srs = stf (tid2rc M.! sid) $ toSubRegIndex sr
        mes = makeExtract d s id srs
        id' = max id (newTempId mes)
    in lowerSubRegVirtual stf tid2rc (accIs ++ mes, id') mis

-- This assumes that REG_SEQUENCE instructions have power-of-two number of uses
lowerSubRegVirtual stf tid2rc (accIs, id) (mi @
  MachineSingle {msOperands = d : us} : mis)
  | isMachineRegSequence mi =
    let rc       = tid2rc M.! (mtId d)
        cs       = [(t, reverse (stf rc (toSubRegIndex sr)))
                   | [t, sr] <- chunksOf 2 us]
    in if any isCustomSubRegIndex (concatMap snd cs) then
      -- custom sub-register indices must be lowered in the 'preProcess' phase
      lowerSubRegVirtual stf tid2rc (accIs ++ [mi], id) mis
    else
         let mcs = makeCombine d id (split2 cs)
             id' = max id (newTempId mcs)
         in lowerSubRegVirtual stf tid2rc (accIs ++ mcs, id') mis

lowerSubRegVirtual stf tid2rc (accIs, id) (mi : is) =
  lowerSubRegVirtual stf tid2rc (accIs ++ [mi], id) is

lowerSubRegVirtual _ _ (is, acc) [] = (is, acc)

isCustomSubRegIndex CustomSubRegIndex = True
isCustomSubRegIndex _ = False


-- Produce a chain of lows and highs recursively
makeExtract d s _ [sr] =
  let mi = mkMachineExtract sr [d, s]
  in [mi]
makeExtract d s id (sr:srs) =
  let d'  = mkSimpleMachineTemp id
      mis = makeExtract d' s (id + 1) srs
      mi  = mkMachineExtract sr [d, d']
  in mis ++ [mi]

mkMachineExtract sr mos =
  let opcode = case sr of
                 LowSubRegIndex  -> LOW
                 HighSubRegIndex -> HIGH
                 CopySubRegIndex -> COPY
  in mkMachineSingle (mkMachineVirtualOpc opcode) [] mos

-- Produce a tree of combines recursively
makeCombine d _ (left @ [_], right @ [_]) =
  let (low, high) = orderCombineOperands (left, right)
  in [mkMachineCombine [d, fst $ head low, fst $ head high]]
makeCombine d id (left, right) =
  let (low, high) = orderCombineOperands (left, right)
      subTree = split2 . map (second tail)
      dl      = mkSimpleMachineTemp id
      lowMis  = makeCombine dl id (subTree low)
      id'     = newTempId lowMis
      dh      = mkSimpleMachineTemp id'
      highMis = makeCombine dh id' (subTree high)
  in lowMis ++ highMis ++ [mkMachineCombine [d, dl, dh]]

-- Select the low combine operand as the left child
orderCombineOperands (left, right) =
  let topSubRegIndex = head . snd . head
  in case mapTuple topSubRegIndex (left, right) of
       (LowSubRegIndex, HighSubRegIndex) -> (left, right)
       (HighSubRegIndex, LowSubRegIndex) -> (right, left)

mkMachineCombine = mkMachineSingle (mkMachineVirtualOpc COMBINE) []

newTempId mis =
  let ids = concatMap (\mi -> [ id | MachineTemp {mtId = id} <- msOperands mi]) mis
  in (maximum ids) + 1

split2 l =
  case chunksOf (length l `div` 2) l of
    [l1, l2] -> (l1, l2)
