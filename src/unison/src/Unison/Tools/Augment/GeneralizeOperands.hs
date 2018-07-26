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

-- (combine) operations with the same used temporaries must get different
-- operand identifiers to be able to apply the alignment constraints.
generalizeOperandsInInstr (id, code)
  o @ SingleOperation {oOpr = Virtual ci @
                              Combine {oCombineLowU = lu, oCombineHighU = hu,
                                       oCombineD = d}} =
  let [lu', hu', d'] =
        map (\(pid, t) -> toSingletonChoice pid t) (zip [id..] [lu, hu, d])
      o'  = o {oOpr = Virtual ci {oCombineLowU = lu', oCombineHighU = hu',
                                  oCombineD = d'}}
  in (id + 3, code ++ [o'])

generalizeOperandsInInstr (id, code) o =
  let t2p = zip (nub $ tUses [o] ++ tDefs [o]) [id..]
      gf  = map $ generalizeOperand (M.fromList t2p)
      id' = if null t2p then id else snd (last t2p) + 1
      o'  = mapToOperands gf gf o
  in (id', code ++ [o'])

generalizeOperand t2p t
  | M.member t t2p = toSingletonChoice (t2p M.! t) t
  | otherwise      = t

toSingletonChoice p t = mkMOperand p (toSingleton $ undoPreAssign t) (tReg t)
