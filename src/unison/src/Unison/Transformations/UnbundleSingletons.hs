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
module Unison.Transformations.UnbundleSingletons
    (unbundleSingletons) where

import Unison.Base
import Unison.Util

unbundleSingletons f _ = mapToOperation unbundleSingleton f

unbundleSingleton Bundle {bundleOs = [o]} = o
unbundleSingleton o = o
