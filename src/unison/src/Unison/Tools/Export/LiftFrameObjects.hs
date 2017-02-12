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
module Unison.Tools.Export.LiftFrameObjects (liftFrameObjects) where

import Data.List
import qualified Data.Map as M

import Unison
import Unison.Analysis.FrameOffsets
import MachineIR

-- This pass lifts frame objects created by spilling to the list of fixed
-- stack frame objects.

liftFrameObjects f @ Function {fCode = code, fFixedStackFrame = fobjs} _target =
  let mobjs  = nub $ concatMap machineFrameObjects $ flatten code
      fstIdx = newFrameIndex fobjs
      objMap = M.fromList [(mfo, toFrameObject idx mfo) |
                           (idx, mfo) <- zip [fstIdx..] mobjs]
      code'  = mapToOperationInBlocks (toFrameIndexOperand objMap) code
      (_, fobjs') = mapAccumL allocateObject (slotSet fobjs) (M.elems objMap)
  in f {fCode = code', fFixedStackFrame = fobjs ++ fobjs'}

machineFrameObjects o = [mo | (Bound mo) <- oAllOps o, isMachineFrameObject mo]

toFrameObject idx (MachineFrameObject _ size align) =
  mkFrameObject idx 0 size align

toFrameIndexOperand ::  Ord r => M.Map (MachineOperand r) FrameObject ->
                                 BlockOperation i r -> BlockOperation i r
toFrameIndexOperand = mapToOperandIf always . toFrameIndex

toFrameIndex objMap (Bound mfo) | isMachineFrameObject mfo =
  mkBound (mkMachineFrameIndex (foIndex (objMap M.! mfo)) True 0)
toFrameIndex _ op = op
