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
module Unison.Transformations.SortGlobalTemps (sortGlobalTemps) where

import Unison.Base
import Unison.Util

sortGlobalTemps f @ Function {fCode = code} _target =
    let code' = map sortEdgeTemps code
    in f {fCode = code'}

sortEdgeTemps b @ Block {bCode = code} = b {bCode = (sortOut . sortIn) code}