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
    (isNVJmp, isCmp, isCmpInstr, isJmp, isJmpInstr) where

import MachineIR
import qualified Unison.Target.Hexagon.SpecsGen as SpecsGen
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl

instance Read HexagonInstruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

isNVJmp = isSingleTargetOf [J2_jumpt_nv, J2_jumpf_nv]

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
