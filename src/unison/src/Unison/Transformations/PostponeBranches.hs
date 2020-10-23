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
module Unison.Transformations.PostponeBranches (postponeBranches) where

import Unison.Base
import Unison.Util
import Unison.Predicates

postponeBranches f @ Function {fCode = code} _target =
    let code' = map postponeBranchesInBlock code
    in f {fCode = code'}

postponeBranchesInBlock b @ Block {bCode = code} =
  let isTerm = isTerminator code
      b1 = moveOperations isTerm before isOut b
      b2 = if any (isTailCallFun code) code
           then moveOperations isTailCall before isTerm b1
           else b1
  in b2
