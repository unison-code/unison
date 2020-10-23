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
module Unison.Tools.Export.ComputeFrameOffsets (computeFrameOffsets) where

import Data.List

import Unison
import Unison.Analysis.FrameOffsets

-- This pass computes the offsets for the variable ("free") frame objects
-- and shifts all offsets according to the stack pointer offset.

computeFrameOffsets f @ Function {fFixedStackFrame = fobjs, fStackFrame = objs,
                                  fStackPointerOffset = off} _ =
  let (_, objs') = mapAccumL allocateObject (slotSet fobjs) objs
      fobjs'   = map (reoffset (- off)) fobjs
      objs''   = map (reoffset (- off)) objs'
  in f {fFixedStackFrame = fobjs', fStackFrame = objs'',
        fStackPointerOffset = 0}
