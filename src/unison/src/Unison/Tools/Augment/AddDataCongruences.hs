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
module Unison.Tools.Augment.AddDataCongruences (addDataCongruences) where

import Data.List
import Data.Maybe

import Unison

addDataCongruences f @ Function {fCode = code, fCongruences = cs} _target =
  let dataCs = concatMap blockDataCongruences code
      cs'    = cs ++ dataCs
      cs''   = sort cs'
  in f {fCongruences = cs''}

blockDataCongruences Block {bCode = code} =
  let ps = concat [[p | p <- oAllOps i, isSingletonChoice p] | i <- code]
  in concatMap (dataCongruences code) ps

dataCongruences code p = concatMap (instructionDataCongruence p) code

instructionDataCongruence p = mapMaybe (operandDataCongruence p) . oUseOperands

operandDataCongruence p q
  | p `isEquivalentTo` q && p /= q = Just $ mapTuple toCongruenceOp (p, q)
  | otherwise = Nothing

isSingletonChoice MOperand {altTemps = [_]} = True
isSingletonChoice _ = False