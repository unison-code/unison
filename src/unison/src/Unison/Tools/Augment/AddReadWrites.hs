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
module Unison.Tools.Augment.AddReadWrites (addReadWrites) where

import Data.List

import Unison
import Unison.Target.API

addReadWrites f @ Function {fCode = code} target =
    let rwif     = fromOperation (readWriteInfo target)
        fcode    = flatten code
        code'    = mapToOperationInBlocks (addReadWritesToOpr rwif) code
        code''   = mapToOperationInBlocks (addProgramCounterWrites) code'
        code'''  = mapToOperationInBlocks (addFunMemWrites) code''
        code'''' = mapToOperationInBlocks (addControlReadWrites fcode) code'''
    in f {fCode = code''''}

addReadWritesToOpr rwif o =
    let (ris, wis) = rwif o
        o'         = addReads ris o
    in addWrites wis o'

addFunMemWrites o
  | isFun o   = addWrites [memObject] o
  | otherwise = o

addProgramCounterWrites o
  | isFun o || isBranch o = addWrites [counterObject] o
  | otherwise = o

addControlReadWrites code o
  | isBranch o || isTailCallFun code o = addWrites [controlObject] o
  | isVirtual o = o
  | controlObject `elem` oWriteObjects o = o
  | otherwise = addReads [controlObject] o

-- | The side effects of an operation o are the union of the side effects of all
-- instructions that can implement o. TODO: make side effects depend on the i(o)
-- variables.
fromOperation rwif o =
    let insts = filter isTargetInstruction (oInstructions o)
        rws   = map (rwif . TargetInstruction . oTargetInstr) insts
        crws  = foldl combineReadWrites ([], []) rws
    in crws

combineReadWrites (accRis, accWis) (ris, wis) =
    (mergeLists accRis ris, mergeLists accWis wis)

mergeLists a b = nub (a ++ b)

addReads = mapToReads . (++)
addWrites = mapToWrites . (++)

memObject     = AllMemory
counterObject = ProgramCounterSideEffect
controlObject = ControlSideEffect
