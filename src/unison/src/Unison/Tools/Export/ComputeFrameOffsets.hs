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
module Unison.Tools.Export.ComputeFrameOffsets (computeFrameOffsets) where

import Data.List
import qualified Data.Map as M

import MachineIR
import Unison
import Unison.Analysis.FrameOffsets

-- This pass computes the offsets for the variable ("free") frame objects,
-- shifts all offsets according to the stack pointer offset, and replaces stack
-- size markers by the actual size of the stack.

computeFrameOffsets f @ Function {fCode = code, fFixedStackFrame = fobjs,
                                  fStackFrame = objs,
                                  fStackPointerOffset = off} _ =
  let (_, objs') = mapAccumL allocateObject (slotSet fobjs) objs
      size       = frameSize (fobjs ++ objs')
      mfsToImm = M.fromList
                 [(mkBound mkMachineFrameSize, mkBound (mkMachineImm size))]
      code'    = mapToOperationInBlocks (applyMapToOperands mfsToImm) code
      fobjs'   = map (reoffset (- off)) fobjs
      objs''   = map (reoffset (- off)) objs'
  in f {fCode = code', fFixedStackFrame = fobjs', fStackFrame = objs'',
        fStackPointerOffset = 0}

reoffset delta fo @ FrameObject {foOffset = off} = fo {foOffset = off + delta}
