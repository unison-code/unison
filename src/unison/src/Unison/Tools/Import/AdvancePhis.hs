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
{-# LANGUAGE FlexibleContexts #-}
module Unison.Tools.Import.AdvancePhis (advancePhis) where

import Unison.Base
import Unison.Util
import Unison.Predicates

advancePhis f @ Function {fCode = code} _target =
  let code' = map (moveOperations isPhi after isIn) code
  in f {fCode = code'}
