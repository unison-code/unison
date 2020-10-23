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
module Unison.Tools.Export.LowerFrameSize (lowerFrameSize) where

import qualified Data.Map as M

import MachineIR
import Unison
import Unison.Analysis.FrameOffsets

-- This pass replaces stack size markers by the actual size of the stack.

lowerFrameSize f @ Function {fCode = code, fFixedStackFrame = fobjs,
                             fStackFrame = objs} _ =
  let size     = frameSize (fobjs ++ objs)
      mfsToImm = M.fromList
                 [(mkBound mkMachineFrameSize, mkBound (mkMachineImm size))]
      code'    = mapToOperationInBlocks (applyMapToOperands mfsToImm) code
  in f {fCode = code'}
