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
module Unison.Tools.Import.ConnectCalls (connectCalls) where

import Unison

connectCalls f _ = peephole connectCall f

connectCall _ (c : f : os) _ | (isCall c || isTailCall c) && isFun f =
  let f' = mapToAttrCall (const (Just (oId c))) f
  in (os, [c, f'])

connectCall _ (o : os) _ = (os, [o])
