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
module Unison.Tools.Augment.GeneralizeOperands (generalizeOperands) where

import Data.List
import qualified Data.Map as M

import Common.Util

import Unison

generalizeOperands f @ Function {fCode = code} _ =
  let (_, code') = foldl generalizeOperandsInBlock (0, []) code
  in f {fCode = code'}

generalizeOperandsInBlock (id, fcode) b @ Block {bCode = code} =
  let (id', code') = foldl generalizeOperandsInInstr (id, []) code
  in (id', fcode ++ [b {bCode = code'}])

generalizeOperandsInInstr (id, code) i =
  let t2p = zip (nub $ tUses [i] ++ tDefs [i]) [id..]
      gf  = map $ generalizeOperand (M.fromList t2p)
      id' = if null t2p then id else snd (last t2p) + 1
      i'  = mapToOperands gf gf i
  in (id', code ++ [i'])

generalizeOperand t2p t
  | M.member t t2p = toSingletonChoice (t2p M.! t) t
  | otherwise      = t

toSingletonChoice p t = mkMOperand p (toSingleton $ undoPreAssign t) (tReg t)
