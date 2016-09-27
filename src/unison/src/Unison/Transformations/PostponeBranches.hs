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
{-# LANGUAGE FlexibleContexts #-}
module Unison.Transformations.PostponeBranches (postponeBranches) where

import Unison.Base
import Unison.Util
import Unison.Predicates

postponeBranches f @ Function {fCode = code} _target =
    let code' = map (moveOperations isBranch before isOut) code
    in f {fCode = code'}
