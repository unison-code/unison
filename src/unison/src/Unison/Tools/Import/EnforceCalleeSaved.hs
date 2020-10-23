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
module Unison.Tools.Import.EnforceCalleeSaved (enforceCalleeSaved) where

import Unison
import Unison.Target.API

enforceCalleeSaved f target =
    let csr = map (mkRegister . mkTargetRegister) $ calleeSaved target
    in if hasReturn (fCode f) then addLongLifeRegs returnBlockIds csr f else f

hasReturn = not . null . returnBlockIds
