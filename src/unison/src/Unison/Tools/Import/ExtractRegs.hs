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
module Unison.Tools.Import.ExtractRegs (extractRegs) where

import Data.List
import qualified Data.Map as M

import Unison

-- | This pass assumes that all registers in the code are only defined once
extractRegs f @ Function {fCode = code} _target =
    let fcode  = flatten code
        entry  = head fcode
        exits  = map blockOut (exitBlocks code)
        r2t    = zip (nub (regs fcode)) [(newTempIndex fcode)..]
        code'  = map (replaceRegsInBB r2t) code
        pas    = concatMap (toPreAssignment (entry : exits)) r2t
        code'' = preAssignTemps pas code'
    in f {fCode = code''}

replaceRegsInBB r2t b @ Block {bCode = code} =
    b {bCode = map (replaceRegs (M.fromList r2t)) code}

replaceRegs :: Ord r => M.Map (Operand r) TemporaryId ->
               BlockOperation i r -> BlockOperation i r
replaceRegs = mapToOperandIf isRegister . replaceReg

replaceReg r2i r =
    case M.lookup r r2i of
      Just t -> mkTemp t
      Nothing -> r

toPreAssignment is (r, t) =
    let ids = concatMap (idIfElem r) is
    in [(o, mkTemp t, r) | o <- ids]

idIfElem r i = [oId i | r `elem` oAllOps i]
