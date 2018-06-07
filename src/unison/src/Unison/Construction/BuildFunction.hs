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
module Unison.Construction.BuildFunction (buildFunction) where

import Data.List
import Data.Maybe

import MachineIR.Base
import MachineIR.Predicates
import Unison.Base
import Unison.Util
import Unison.Constructors
import Unison.Target.API

buildFunction :: Read i => Read r =>
                 TargetWithOptions i r rc s -> MachineFunction i r ->
                 Function i r
buildFunction target MachineFunction {mfName = name, mfBlocks = blocks,
                                      mfProperties = mps, mfIR = ir} =
  let itf  = instructionType target
      oif  = operandInfo target
      code = snd $ foldl (buildBlock itf oif) (0, []) blocks
      ffos = buildFixedStackFrame mps
      fos  = buildStackFrame mps
      cs   = buildConstants mps
      jt   = buildJumpTable (find isMachineFunctionPropertyJumpTable mps)
      rfs  = buildRemovedFreqs (find isMachineFunctionPropertyRemovedFreqs mps)
      f    = mkCompleteFunction [] name code [] [] ffos fos 0 0 cs jt [] rfs ir
  in f

buildBlock itf oif (id, code)
  MachineBlock {mbId = mid, mbProperties = mps, mbInstructions = mis} =
  let (id', icode) = foldl zipId (id, []) mis
      attrs        = buildBlockAttributes mps
      is           = map (fromMachineInstruction itf oif) icode
      b            = mkBlock mid attrs is
  in (id', code ++ [b])

zipId (id, icode) i =
    let icode' = icode ++ [(id, i)]
        id'    = id + case i of
                        MachineSingle {} -> 1
                        MachineBundle {mbInstrs = mis} -> toInteger $ length mis
    in (id', icode')

buildBlockAttributes mps =
  let f = fmap mbPropertyFreq $ find isMachineBlockPropertyFreq mps
  in mkNullBlockAttributes {aFreq = f}

buildFixedStackFrame =
    map toStackFrameObject . fromMaybe [] . fmap mfPropertyFixedFrame .
        find isMachineFunctionPropertyFixedFrame

buildStackFrame =
    map toStackFrameObject . fromMaybe [] . fmap mfPropertyFrame .
        find isMachineFunctionPropertyFrame

toStackFrameObject MachineFrameObjectInfo {
                         mfoiIndex = idx, mfoiOffset = off,
                         mfoiSize = size, mfoiAlignment = align,
                         mfoiCSRegister = csreg} =
  mkFrameObject idx off size align csreg

buildJumpTable Nothing = ("", [])
buildJumpTable (Just (MachineFunctionPropertyJumpTable k es)) =
  (k, map buildJumpTableEntry es)

buildJumpTableEntry (MachineJumpTableEntry id bs) =
  mkJumpTableEntry id (map mbrId bs)

buildRemovedFreqs Nothing = []
buildRemovedFreqs (Just rfs) = mfPropertyRemovedFreqs rfs

buildConstants mps =
  case find isMachineFunctionPropertyConstants mps of
   Nothing -> []
   Just cs -> mfPropertyConstants cs
