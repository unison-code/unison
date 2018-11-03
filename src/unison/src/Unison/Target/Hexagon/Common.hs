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
module Unison.Target.Hexagon.Common
    (singleIssue, preserveDominatedIns, isNVJmpInstr, isNVJmp, isCmp, isCmpInstr,
     isJmp, isJmpInstr,
     isLinearJump, isLinearNewValueCmpJump, isNewValueCmpJump, isJumpNew,
     isMemAccessWithOff, memAccessAlignment, isOldValueStoreInstr,
     newValueStoreInstr, isMuxTransferInstr, isCondTransferInstr,
     condTransferInstr, muxTransferInstr, isRematerializable,
     isSourceInstr, isDematInstr, isRematInstr, sourceInstr, dematInstr,
     rematInstr, originalInstr, constantExtendedInstr, nonConstantExtendedInstr,
     isConstantExtended, mayLoad', mayStore', spillInstrs) where

import Data.List
import qualified Data.Map as M

import Unison
import MachineIR
import Unison.Target.Query
import qualified Unison.Target.Hexagon.SpecsGen as SpecsGen
import qualified Unison.Target.API as API
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl

singleIssue to = API.isBoolOption "single-issue" to

preserveDominatedIns to = API.isBoolOption "preserve-dominated-ins" to

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

isOldValueStoreInstr i = M.member i newValueStoreVersions

newValueStoreInstr i = newValueStoreVersions M.! i

newValueStoreVersions = M.fromList
  [(S2_storerb_io, S2_storerbnew_io),
   (S2_storerb_pi, S2_storerbnew_pi),
   (S2_storerh_io, S2_storerhnew_io),
   (S2_storerh_pi, S2_storerhnew_pi),
   (S2_storeriabs, S2_storerinewabs),
   (S2_storeri_io, S2_storerinew_io),
   (S2_storeri_io_fi, S2_storerinew_io_fi),
   (S2_storeri_pi, S2_storerinew_pi),
   (S4_storerh_rr, S4_storerhnew_rr),
   (S4_storeri_rr, S4_storerinew_rr),
   (S2_storerb_io_ce, S2_storerbnew_io_ce),
   (S2_storerb_pi_ce, S2_storerbnew_pi_ce),
   (S2_storerh_io_ce, S2_storerhnew_io_ce),
   (S2_storerh_pi_ce, S2_storerhnew_pi_ce),
   (S2_storeriabs_ce, S2_storerinewabs_ce),
   (S2_storeri_io_ce, S2_storerinew_io_ce),
   (S2_storeri_io_fi_ce, S2_storerinew_io_fi_ce),
   (S2_storeri_pi_ce, S2_storerinew_pi_ce),
   (S4_storerh_rr_ce, S4_storerhnew_rr_ce),
   (S4_storeri_rr_ce, S4_storerinew_rr_ce)]

isMuxTransferInstr i = M.member (i, False) condTransferVersions
isCondTransferInstr i = M.member i (inverseMap condTransferVersions)

condTransferInstr (i, new) = condTransferVersions M.! (i, new)
muxTransferInstr i = (inverseMap condTransferVersions) M.! i

condTransferVersions = M.fromList
  [((C2_mux, False), C2_mux_tfr),
   ((C2_muxii, False), C2_muxii_tfr),
   ((C2_muxii_ce, False), C2_muxii_tfr),
   ((C2_muxir, False), C2_muxir_tfr),
   ((C2_muxir_ce, False), C2_muxir_tfr),
   ((C2_muxri, False), C2_muxri_tfr),
   ((C2_muxri_ce, False), C2_muxri_tfr),
   ((C2_mux, True), C2_mux_tfr_new),
   ((C2_muxii, True), C2_muxii_tfr_new),
   ((C2_muxii_ce, True), C2_muxii_tfr_new),
   ((C2_muxir, True), C2_muxir_tfr_new),
   ((C2_muxir_ce, True), C2_muxir_tfr_new),
   ((C2_muxri, True), C2_muxri_tfr_new),
   ((C2_muxri_ce, True), C2_muxri_tfr_new)]

data RematTriple = RematTriple {
  source :: HexagonInstruction,
  demat  :: HexagonInstruction,
  remat  :: HexagonInstruction}

isRematerializable i = M.member i rematVersions
isSourceInstr = isRInstrOf source
isDematInstr = isRInstrOf demat
isRematInstr = isRInstrOf remat
isRInstrOf f i = i `elem` [f t | t <- M.elems rematVersions]
sourceInstr i = source $ rematVersions M.! i
dematInstr  i = demat $ rematVersions M.! i
rematInstr  i = remat $ rematVersions M.! i

originalInstr i = (M.fromList (concat originalInstrLists)) M.! i

originalInstrLists =
  [[(i, i), (source ris, i), (demat ris, i), (remat ris, i)]
  | (i, ris) <- M.toList rematVersions]


rematVersions = M.fromList
  [(A2_tfrsi, RematTriple A2_tfrsi_source A2_tfrsi_demat A2_tfrsi_remat),
   (A2_tfrsi_ce, RematTriple A2_tfrsi_source_ce A2_tfrsi_demat_ce A2_tfrsi_remat_ce),
   (A2_tfrpi, RematTriple A2_tfrpi_source A2_tfrpi_demat A2_tfrpi_remat),
   (L2_loadri_io_fi, RematTriple L2_loadri_io_fi_source_fi L2_loadri_io_fi_demat_fi L2_loadri_io_fi_remat_fi),
   (L2_loadrb_io_fi, RematTriple L2_loadrb_io_fi_source_fi L2_loadrb_io_fi_demat_fi L2_loadrb_io_fi_remat_fi),
   (L2_loadruh_io_fi, RematTriple L2_loadruh_io_fi_source_fi L2_loadruh_io_fi_demat_fi L2_loadruh_io_fi_remat_fi),
   (L4_loadrb_abs_ce, RematTriple L4_loadrb_abs_source_ce L4_loadrb_abs_demat_ce L4_loadrb_abs_remat_ce),
   (L4_loadri_abs_ce, RematTriple L4_loadri_abs_source_ce L4_loadri_abs_demat_ce L4_loadri_abs_remat_ce),
   (L4_loadrh_abs_ce, RematTriple L4_loadrh_abs_source_ce L4_loadrh_abs_demat_ce L4_loadrh_abs_remat_ce),
   (L4_loadruh_abs_ce, RematTriple L4_loadruh_abs_source_ce L4_loadruh_abs_demat_ce L4_loadruh_abs_remat_ce),
   (L4_loadrub_abs_ce, RematTriple L4_loadrub_abs_source_ce L4_loadrub_abs_demat_ce L4_loadrub_abs_remat_ce),
   (TFR_FI_fi, RematTriple TFR_FI_fi_source_fi TFR_FI_fi_demat_fi TFR_FI_fi_remat_fi),
   (L2_loadrigp, RematTriple L2_loadrigp_source L2_loadrigp_demat L2_loadrigp_remat),
   (L2_loadrhgp, RematTriple L2_loadrhgp_source L2_loadrhgp_demat L2_loadrhgp_remat),
   (L2_loadrubgp, RematTriple L2_loadrubgp_source L2_loadrubgp_demat L2_loadrubgp_remat),
   (L2_loadruhgp, RematTriple L2_loadruhgp_source L2_loadruhgp_demat L2_loadruhgp_remat),
   (CONST64_Int_Real, RematTriple CONST64_Int_Real_source CONST64_Int_Real_demat CONST64_Int_Real_remat),
   (L2_loadrd_io_fi, RematTriple L2_loadrd_io_fi_source_fi L2_loadrd_io_fi_demat_fi L2_loadrd_io_fi_remat_fi),
   (L4_loadrd_abs_ce, RematTriple L4_loadrd_abs_source_ce L4_loadrd_abs_demat_ce L4_loadrd_abs_remat_ce),
   (TFR_PdFalse, RematTriple TFR_PdFalse_source TFR_PdFalse_demat TFR_PdFalse_remat),
   (TFR_PdTrue, RematTriple TFR_PdTrue_source TFR_PdTrue_demat TFR_PdTrue_remat)]

constantExtendedInstr :: HexagonInstruction -> HexagonInstruction
constantExtendedInstr i = read $ show i ++ "_ce"

nonConstantExtendedInstr :: HexagonInstruction -> HexagonInstruction
nonConstantExtendedInstr i = read $ dropSuffix "_ce" (show i)

isConstantExtended :: HexagonInstruction -> Bool
isConstantExtended i = "_ce" `isSuffixOf` (show i)

mayLoad' = mayLoad SpecsGen.readWriteInfo

mayStore' STW_nv = True
mayStore' i = mayStore (SpecsGen.readWriteInfo) i

spillInstrs = [STW, STD, STW_nv, LDW, LDD]
