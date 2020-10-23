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
module Unison.Transformations.UnbundleSingletons
    (unbundleSingletons) where

import Unison.Base
import Unison.Util

unbundleSingletons f _ = mapToOperation unbundleSingleton f

unbundleSingleton Bundle {bundleOs = [o]} = o
unbundleSingleton o = o
