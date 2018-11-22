{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Export.LiftFrameObjects (liftFrameObjects) where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Unison
import Unison.Analysis.FrameOffsets
import MachineIR

-- This pass lifts frame objects created by spilling to the list of fixed
-- stack frame objects.

liftFrameObjects f @ Function {fCode = code, fFixedStackFrame = fobjs} _target =
  let mobjs  = nub $ concatMap machineFrameObjects $ flatten code
      gobjs  = groupBy overlap $ sortBy (comparing mfoAtoms) mobjs
      fstIdx = newFrameIndex fobjs
      o2i    = M.fromList $
               concat [[(a, (idx, (mfoAtomUnion mfos)))
                       | a <- mfoAtomUnion mfos]
                      | (idx, mfos) <- zip [fstIdx..] gobjs]
      code'  = mapToOperationInBlocks (toFrameIndexOperand o2i) code
      fobjs' = map (toFrameObject o2i . largest) gobjs
      (_, fobjs'') = mapAccumL allocateObject (slotSet fobjs) fobjs'
  in f {fCode = code', fFixedStackFrame = fobjs ++ fobjs''}

machineFrameObjects o = [mo | (Bound mo) <- oAllOps o, isMachineFrameObject mo]

mfoAtoms (MachineFrameObject o (Just s) _ _) = [o..o+s-1]

mfoAtomUnion mfos = S.toList $ S.fromList $ concatMap mfoAtoms mfos

overlap mfo1 mfo2 =
  let atomSet = S.fromList . mfoAtoms
  in not $ S.null $ S.intersection (atomSet mfo1) (atomSet mfo2)

largest mfos = last $ sortBy (comparing (\mfo -> length (mfoAtoms mfo))) mfos

toFrameIndexOperand = mapToOperandIf always . toFrameIndex

toFrameIndex o2i (Bound mfo) | isMachineFrameObject mfo =
  let o       = mfoOffset mfo
      (i, as) = o2i M.! o
      idx     = toInteger $ fromJust $ elemIndex o as
      -- assume the highest part of a fr. obj. is stored in the lowest position
      off     = toInteger (length as) - (fromJust $ mfoSize mfo) - idx
      fi      = mkBound (mkMachineFrameIndex i True off)
  in fi
toFrameIndex _ op = op

toFrameObject o2i (MachineFrameObject o (Just size) align _) =
  let idx = fst $ o2i M.! o
  in mkFrameObject idx 0 (Just size) align Nothing
