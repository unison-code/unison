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
module Unison.Tools.Augment.GeneralizeCongruences (generalizeCongruences) where

import Data.List
import Data.Maybe

import Unison

generalizeCongruences f @ Function {fCode = code, fCongruences = cs} _ =
    let fcode = flatten code
        cs'   = map (generalizeCongruence fcode) cs
    in f {fCongruences = cs'}

generalizeCongruence code (t, t') = (toUseOperand code t, toDefOperand code t')

toUseOperand code t =
    let i = fromJust $ find isOut (users t code)
        p = equivalentTo t (oUses i)
    in mkOperandRef p

toDefOperand code t =
    let i = definer t code
        p = equivalentTo t (oDefs i)
    in mkOperandRef p

equivalentTo t = operandId . fromJust . find (isEquivalentTo t)