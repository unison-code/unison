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
  let idxToOff = M.fromList [(foIndex fo, foOffset fo) | fo <- objs]
  in mapToOperationInBlocks
     (mapToOperandIf (isFixedType fixed) (liftFI idxToOff)) code

isFixedType fixed (Bound (MachineFrameIndex _ fixed' _)) = fixed == fixed'
isFixedType _ _ = False

liftFI idxToOff (Bound (MachineFrameIndex idx _ off)) =
  mkBound (mkMachineImm $ (idxToOff M.! idx) + off)
