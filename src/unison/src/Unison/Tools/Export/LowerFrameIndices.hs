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
module Unison.Tools.Export.LowerFrameIndices (lowerFrameIndices) where

import qualified Data.Map as M

import MachineIR
import Unison

-- This pass replaces stack frame object indices in the code by actual
-- immediates.

lowerFrameIndices f @ Function {fCode = code, fFixedStackFrame = fobjs,
                                fStackFrame = objs} _target =
  let code'    = replaceFIsByImms True fobjs code
      code''   = replaceFIsByImms False objs code'
  in f {fCode = code''}

replaceFIsByImms fixed objs code =
  let idxToOff = M.fromList [(mkBound (mkMachineFrameIndex (foIndex fo) fixed),
                              mkBound (mkMachineImm (foOffset fo)))
                            | fo <- objs]
  in mapToOperationInBlocks (applyMapToOperands idxToOff) code
