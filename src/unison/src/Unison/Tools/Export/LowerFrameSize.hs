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
module Unison.Tools.Export.LowerFrameSize (lowerFrameSize) where

import qualified Data.Map as M

import MachineIR
import Unison
import Unison.Analysis.FrameOffsets

-- This pass replaces stack size markers by the actual size of the stack.

lowerFrameSize f @ Function {fCode = code, fFixedStackFrame = fobjs,
                             fStackFrame = objs, fStackPointerOffset = off} _ =
  let size     = frameSize (fobjs ++ objs) - off
      mfsToImm = M.fromList
                 [(mkBound mkMachineFrameSize, mkBound (mkMachineImm size))]
      code'    = mapToOperationInBlocks (applyMapToOperands mfsToImm) code
  in f {fCode = code', fStackPointerOffset = 0}
