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
module Unison.Transformations.SortGlobalTemps (sortGlobalTemps) where

import Unison.Base
import Unison.Util

sortGlobalTemps f @ Function {fCode = code} _target =
    let code' = map sortEdgeTemps code
    in f {fCode = code'}

sortEdgeTemps b @ Block {bCode = code} = b {bCode = (sortOut . sortIn) code}