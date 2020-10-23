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
module Unison.Transformations.PropagatePhiCongruences
       (propagatePhiCongruences) where

import Common.Util

import Unison.Base
import Unison.Util
import Unison.Predicates

propagatePhiCongruences f @ Function {fCode = code} _target =
    let phis        = concatMap collectPhis code
        sourcedCode = foldl sourceDef code phis
        sinkedCode  = foldl sinkUses sourcedCode phis
        cs          = concatMap duplicateTuple phis
    in f {fCode = sinkedCode, fCongruences = cs}

collectPhis Block {bLab = l, bCode = code} =
    [(l, phiUses i, fromSingleton $ oDefs i) | i <- code, isPhi i]

sourceDef code (l, _, d) = applyToBlock (addToIn [d]) code l

sinkUses code (_, us, _) = foldl sinkUse code us
sinkUse code (u, p) = applyToBlock (addToOut [u]) code p

duplicateTuple (_, us, d) = [(u, d) | (u, _) <- us]
