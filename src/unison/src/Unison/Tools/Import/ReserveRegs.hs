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
module Unison.Tools.Import.ReserveRegs (reserveRegs) where

import Unison
import Unison.Target.API

reserveRegs f target =
    let rr = map (mkRegister . mkTargetRegister) $ reserved target
    in addLongLifeRegs exitBlockIds rr f
