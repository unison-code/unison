{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

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
        let mi1 = toMachineInstruction $ normalizePhiUses $
                  fromMachineInstruction itf oif (-1, mi)
            mos = map maybeCopyMtFlags (zip (msOperands mi) (msOperands mi1))
            mi2 = mi1 {msOperands = mos}
        in mi2
    | otherwise = mi

normalizePhiUses o =
    let us = concat [[t, mkBlockRef bid] | (t, bid) <- nub $ phiUses o]
    in mapToOperands (const us) id o

maybeCopyMtFlags (MachineTemp {mtFlags = mtfs}, mt @ MachineTemp {}) =
  mt {mtFlags = mtfs}
maybeCopyMtFlags (_, mo) = mo
