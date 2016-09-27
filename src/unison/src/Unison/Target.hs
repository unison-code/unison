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
module Unison.Target (unisonTargets) where

import Unison.Target.API (Any(..))
import Unison.Target.Hexagon as Hexagon (target)
import Unison.Target.ARM as ARM (target)
import Unison.Target.Mips as Mips (target)

unisonTargets =
    [("Hexagon", Any Hexagon.target),
     ("ARM", Any ARM.target),
     ("Mips", Any Mips.target)]
