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
module Unison.Tools.Export.DirectFrame (directFrame) where

import Unison
import Unison.Target.API

-- This pass directs the frame (whether the stack grows up or down) as specified
-- by the target.

directFrame f @ Function {fFixedStackFrame = fobjs, fStackFrame = objs} target =
  case stackDirection target of
    StackGrowsDown ->
      let fobjs' = map revertDirection fobjs
          objs'  = map revertDirection objs
      in f {fFixedStackFrame = fobjs', fStackFrame = objs'}
    StackGrowsUp -> f

revertDirection fo @ FrameObject {foOffset = off} =
  let off' = - (off + foMaybeSize fo)
  in fo {foOffset = off'}
