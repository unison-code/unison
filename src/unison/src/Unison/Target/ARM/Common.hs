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
module Unison.Target.ARM.Common
    (unitLatency, isCpsrDef, toExplicitCpsrDef, fromExplicitCpsrDef,
     defaultMIRPred, defaultUniPred, isRematerializable, isSourceInstr,
     isDematInstr, isRematInstr, sourceInstr, dematInstr, rematInstr,
     originalInstr, spillInstrs) where

import qualified Data.Map as M
import Data.Tuple

import MachineIR
import Unison
import qualified Unison.Target.API as API
import Unison.Target.ARM.SpecsGen.ARMInstructionDecl

unitLatency to = API.isBoolOption "unit-latency" to

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

defaultMIRPred = [mkMachineImm 14, mkMachineNullReg]

defaultUniPred = map mkBound defaultMIRPred

data RematTriple = RematTriple {
  source :: ARMInstruction,
  demat  :: ARMInstruction,
  remat  :: ARMInstruction}

isRematerializable i = M.member i rematVersions
isSourceInstr = isRInstrOf source
isDematInstr = isRInstrOf demat
isRematInstr = isRInstrOf remat
isRInstrOf f i = i `elem` [f t | t <- M.elems rematVersions]
sourceInstr i = source $ rematVersions M.! i
dematInstr  i = demat $ rematVersions M.! i
rematInstr  i = remat $ rematVersions M.! i

originalInstr i =
  (M.fromList [(remat ris, i) | (i, ris) <- M.toList rematVersions]) M.! i

rematVersions = M.fromList
  [(T2MOVi, RematTriple T2MOVi_source T2MOVi_demat T2MOVi_remat),
   (T2MOVi16, RematTriple T2MOVi16_source T2MOVi16_demat T2MOVi16_remat),
   (T2MOVi32imm, RematTriple T2MOVi32imm_source T2MOVi32imm_demat T2MOVi32imm_remat),
   (TMOVi8s, RematTriple TMOVi8s_source TMOVi8s_demat TMOVi8s_remat),
   (FMSTAT_cpsr, RematTriple FMSTAT_cpsr_source FMSTAT_cpsr_demat FMSTAT_cpsr_remat),
   (T2ADDri_fi, RematTriple T2ADDri_fi_source_fi T2ADDri_fi_demat_fi T2ADDri_fi_remat_fi),
   (T2LDRi12_fi, RematTriple T2LDRi12_fi_source_fi T2LDRi12_fi_demat_fi T2LDRi12_fi_remat_fi),
   (T2LEApcrelJT, RematTriple T2LEApcrelJT_source T2LEApcrelJT_demat T2LEApcrelJT_remat),
   (T2MVNi, RematTriple T2MVNi_source T2MVNi_demat T2MVNi_remat),
   (VLDRD_cpi, RematTriple VLDRD_cpi_source_cpi VLDRD_cpi_demat_cpi VLDRD_cpi_remat_cpi),
   (VLDRD_fi, RematTriple VLDRD_fi_source_fi VLDRD_fi_demat_fi VLDRD_fi_remat_fi),
   (VLDRS_cpi, RematTriple VLDRS_cpi_source_cpi VLDRS_cpi_demat_cpi VLDRS_cpi_remat_cpi),
   (T2LDRBi12_fi, RematTriple T2LDRBi12_fi_source_fi T2LDRBi12_fi_demat_fi T2LDRBi12_fi_remat_fi),
   (T2LEApcrel_cpi, RematTriple T2LEApcrel_cpi_source_cpi T2LEApcrel_cpi_demat_cpi T2LEApcrel_cpi_remat_cpi),
   (VLDRS_fi, RematTriple VLDRS_fi_source_fi VLDRS_fi_demat_fi VLDRS_fi_remat_fi)]

spillInstrs = [STORE, STORE_T, STORE_D, LOAD, LOAD_T, LOAD_D]
