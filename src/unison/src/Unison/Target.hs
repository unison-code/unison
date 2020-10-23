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
module Unison.Target (unisonTargets) where

import Unison.Target.API (Any(..))
import Unison.Target.Hexagon as Hexagon (target)
import Unison.Target.ARM as ARM (target)
import Unison.Target.Mips as Mips (target)

unisonTargets =
    [("Hexagon", Any Hexagon.target),
     ("ARM", Any ARM.target),
     ("Mips", Any Mips.target)]
