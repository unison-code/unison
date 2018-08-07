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
module Unison.Tools.Export.AssignRegisters (assignRegisters) where

import Data.Maybe
import Data.List
import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Target.RegisterArray
import qualified Unison.Graphs.CG as CG

import Unison.Analysis.TemporaryType

assignRegisters tight registers f @ Function {fCode = code} target =
    let oif   = operandInfo target
        fCode = flatten code
        ra    = mkRegisterArray target 0
        ts    = sort (tUniqueOps fCode)
        cg    = CG.fromFunction f
        _     = CG.toDot cg
        t2w   = tempWidths ra oif fCode cg
        inf   = maxTempWidth tight code t2w
        ra'   = mkRegisterArray target inf
        t2aw  = M.fromList (zip ts (zip registers (M.elems t2w)))
        aw2r  = atomWidthToRegs ra'
        t2r   = M.map (toRegOrNull aw2r) t2aw
        code' = mapToOperationInBlocks (mapToModelOperand (applyMapToChoice t2r)) code
    in f {fCode = code'}

toRegOrNull _ (RegisterAtom (-1), _) = NullTemporary
toRegOrNull aw2r aw =
  case M.lookup aw aw2r of
  Just r -> r
  Nothing -> error ("no register could be found corresponding to the (atom, width) combination " ++ show aw)

applyMapToChoice k2t tc @ MOperand {altTemps = ts} =
    tc {altTemps = map (applyMap' k2t) ts}

applyMap' _ NullTemporary = NullTemporary
applyMap' t2r t =
    let r = fromMaybe t (M.lookup t t2r)
    in preAssign t r
