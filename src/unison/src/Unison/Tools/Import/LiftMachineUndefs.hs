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
module Unison.Tools.Import.LiftMachineUndefs (liftMachineUndefs) where

import Data.Maybe

import MachineIR

liftMachineUndefs mf _target =
  let mf' = mapToMachineBlock liftUndefsInBlock mf
  in mf'

liftUndefsInBlock mb @ MachineBlock {mbInstructions = mis} =
  let mb' = mb {mbInstructions = concatMap liftUndefsInInstr mis}
  in mb'

liftUndefsInInstr mi @ MachineSingle {msOperands = mos} =
  let mids = mapMaybe maybeMkImplicitDef mos
      mi'  = mi {msOperands = map maybeDefineTemp mos}
  in mids ++ [mi']

maybeMkImplicitDef mt
  | isMachineTempUndef mt =
      let mt' = maybeDefineTemp mt
      in Just $ mkMachineSingle (mkMachineVirtualOpc IMPLICIT_DEF) [] [mt']
maybeMkImplicitDef _ = Nothing

maybeDefineTemp mt @ MachineTemp {mtFlags = fs} =
  mt {mtFlags = filter (not . isMachineRegUndef) fs}
maybeDefineTemp mo = mo

isMachineTempUndef MachineTemp {mtFlags = fs} = any isMachineRegUndef fs
isMachineTempUndef _ = False
