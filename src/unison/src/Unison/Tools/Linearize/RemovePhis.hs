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
module Unison.Tools.Linearize.RemovePhis (removePhis) where

import Unison

removePhis f @ Function {fCode = code} _target =
    let code' = filterCode (not . isPhi) code
    in f {fCode = code'}
