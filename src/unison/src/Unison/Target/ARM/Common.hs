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
module Unison.Target.ARM.Common
    (isCpsrDef, toExplicitCpsrDef, fromExplicitCpsrDef) where

import qualified Data.Map as M
import Data.Tuple

import Unison.Target.ARM.SpecsGen.ARMInstructionDecl

isCpsrDef i = i `elem` (map fst cpsrMap)
toExplicitCpsrDef i = (M.fromList cpsrMap) M.! i
fromExplicitCpsrDef i = (M.fromList (map swap cpsrMap)) M.! i

cpsrMap =
  [(TCMPi8, TCMPi8_cpsr),
   (T2CMPri, T2CMPri_cpsr),
   (T2TSTri, T2TSTri_cpsr),
   (T2CMNri, T2CMNri_cpsr),
   (T2CMPrr, T2CMPrr_cpsr),
   (TCMPr,   TCMPr_cpsr),
   (TCMPhir, TCMPhir_cpsr),
   (T2TSTrr, T2TSTrr_cpsr),
   (TTST,    TTST_cpsr),
   (T2CMPrs, T2CMPrs_cpsr),
   (T2TSTrs, T2TSTrs_cpsr),
   (T2SUBrr, T2SUBrr_cpsr),
   (T2SUBri, T2SUBri_cpsr),
   (T2ORRrr, T2ORRrr_cpsr),
   (T2ANDri, T2ANDri_cpsr),
   (T2TEQrr, T2TEQrr_cpsr),
   (FMSTAT, FMSTAT_cpsr),
   (T2ADDri, T2ADDri_cpsr)]
