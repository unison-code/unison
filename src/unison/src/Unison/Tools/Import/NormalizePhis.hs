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
module Unison.Tools.Import.NormalizePhis (normalizePhis) where

import Data.List

import Unison
import Unison.Target.API
import MachineIR

normalizePhis mf target =
  let itf = instructionType target
      oif = operandInfo target
  in mapToMachineInstruction (normalizePhi itf oif) mf

normalizePhi itf oif mi
    | isMachinePhi mi =
        toMachineInstruction $ normalizePhiUses $
        fromMachineInstruction itf oif (-1, mi)
    | otherwise = mi

normalizePhiUses o =
    let us = concat [[t, mkBlockRef bid] | (t, bid) <- nub $ phiUses o]
    in mapToOperands (const us) id o
