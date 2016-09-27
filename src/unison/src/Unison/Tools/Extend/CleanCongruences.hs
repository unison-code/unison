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
module Unison.Tools.Extend.CleanCongruences (cleanCongruences) where

import Unison.Base

cleanCongruences f @ Function {fCongruences = cs} _target =
    let cs' = filter (not . isReflexive) cs
    in f {fCongruences = cs'}

isReflexive (o1, o2) = o1 == o2