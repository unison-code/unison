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
module Unison.Transformations.PropagatePhiCongruences (propagatePhiCongruences) where

import Unison.Base
import Unison.Util

propagatePhiCongruences (Function code []) _target =
    let phis        = concatMap collectPhis code
        sourcedCode = foldl sourceDef code phis
        sinkedCode  = foldl sinkUses sourcedCode phis
        cs          = concatMap duplicateTuple phis
    in Function sinkedCode cs

collectPhis (Block l code) =
    [(l, phiUses i, fromSingleton $ oDefs i) | i <- code, isPhi i]

sourceDef code (l, _, d) = applyToBB (addToIn [d]) l code

sinkUses code (_, us, _) = foldl sinkUse code us
sinkUse code (u, p) = applyToBB (addToOut [u]) p code

duplicateTuple (_, us, d) = [(u, d) | (u, _) <- us]
