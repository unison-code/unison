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
module Unison.Tools.Import.ReserveRegs (reserveRegs) where

import Unison
import Unison.Target.API

reserveRegs f target =
    let rr = map (mkRegister . mkTargetRegister) $ reserved target
    in addLongLifeRegs exitBlockIds rr f
