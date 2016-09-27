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
module Unison.Transformations.UnbundleSingletons
    (unbundleSingletons) where

import Unison.Base
import Unison.Util

unbundleSingletons f _ = mapToOperation unbundleSingleton f

unbundleSingleton Bundle {bundleOs = [o]} = o
unbundleSingleton o = o
