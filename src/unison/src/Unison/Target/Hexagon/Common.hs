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
module Unison.Target.Hexagon.Common
    (isNVJmpInstr, isNVJmp, isCmp, isCmpInstr, isJmp, isJmpInstr,
     isLinearJump, isLinearNewValueCmpJump, isNewValueCmpJump, isJumpNew,
     isMemAccessWithOff, memAccessAlignment) where

import Data.List
import qualified Data.Map as M

import MachineIR
import qualified Unison.Target.Hexagon.SpecsGen as SpecsGen
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl

instance Read HexagonInstruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

isNVJmpInstr i = i `elem` nvJmpInstrs

isNVJmp = isSingleTargetOf nvJmpInstrs

nvJmpInstrs = [J2_jumpt_nv, J2_jumpf_nv]

isCmp = isSingleTargetOf cmpInstrs

isCmpInstr i = i `elem` cmpInstrs

cmpInstrs =
  [C2_cmpeq, C2_cmpeqi, C2_cmpeqp, C2_cmpgei, C2_cmpgeui, C2_cmpgt, C2_cmpgti,
   C2_cmpgtp, C2_cmpgtu, C2_cmpgtui, C2_cmpgtup, C4_cmplte, C4_cmpltei,
   C4_cmplteu, C4_cmplteui, C4_cmpneq, C4_cmpneqi]

isJmpInstr i = i `elem` jmpInstrs

isJmp = isSingleTargetOf jmpInstrs

jmpInstrs = [J2_jumpt, J2_jumpf]

isSingleTargetOf is ms =
    isMachineTarget ms && mopcTarget (msOpcode ms) `elem` is

isLinearJump i = i `elem` [J2_jumpt_linear, J2_jumpt_nv_linear, J2_jumpf_linear,
                           J2_jumpf_nv_linear]
isLinearNewValueCmpJump i = "_jumpnv_t_linear" `isSuffixOf` (show i)

isNewValueCmpJump i = "_jumpnv_t" `isSuffixOf` (show i)
isJumpNew i = i `elem` [J2_jumptnew, J2_jumpfnew]

isMemAccessWithOff i = M.member i memAccessAlignments

memAccessAlignment i = memAccessAlignments M.! i

memAccessAlignments = M.fromList
  [(S2_storerd_io, 8),
   (S2_storeri_io, 4),
   (S2_storerinew_io, 4),
   (S2_storerh_io, 2),
   (S2_storerhnew_io, 2),
   (S2_storerb_io, 1),
   (S2_storerbnew_io, 1),
   (L2_loadrd_io, 8),
   (L2_loadri_io, 4),
   (L2_loadrh_io, 2),
   (L2_loadrb_io, 1)]
