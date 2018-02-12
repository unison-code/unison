module Unison.Target.ARM.Usages (usages) where

import Unison

import Unison.Target.ARM.Common
import Unison.Target.ARM.ARMResourceDecl
import qualified Unison.Target.ARM.SpecsGen as SpecsGen
import Unison.Target.ARM.SpecsGen.ARMItineraryDecl
import Unison.Target.ARM.SpecsGen.ARMInstructionDecl

-- | Declares resource usages of each instruction

usages to i =
  let it = SpecsGen.itinerary i
  -- TODO: define instruction size as BundleWidth usage
  in mergeUsages (itineraryUsage' to i it)
     [mkUsage BundleWidth (size i) 1 | size i > 0]

itineraryUsage' to i it =
  let us = itineraryUsage i it
  in if unitLatency to then
       [u {occupation = 1, offset = 0} | u  <- us]
     else us

itineraryUsage i _
  | i `elem` [VMOVScc, VMOVDcc] = concatUsages (ccInstrs i)
itineraryUsage _ it
  | it `elem` [NoItinerary] = []
  | it `elem` [IIC_Br, IIC_iALUi, IIC_iBITi, IIC_iCMPi, IIC_iEXTr,
               IIC_iLoad_bh_r, IIC_iLoad_r, IIC_iMOVi, IIC_iMVNi, IIC_fpCVTID,
               IIC_iALUr, IIC_iCMOVr, IIC_iCMPr, IIC_iLoad_bh_ru, IIC_iMVNr,
               IIC_iStore_r, IIC_iTSTi, IIC_iTSTr, IIC_iUNAsi, IIC_fpLoad32,
               IIC_fpLoad64, IIC_iBITr, IIC_iCMOVi, IIC_iCMOVsr, IIC_iEXTAr,
               IIC_iLoad_iu, IIC_iMAC16, IIC_iStore_bh_iu, IIC_iStore_bh_r,
               IIC_iStore_bh_ru, IIC_iStore_d_r, IIC_iStore_iu, IIC_fpALU32,
               IIC_fpCMP64, IIC_fpCVTDS, IIC_fpMOVID, IIC_iLoad_ru, IIC_iMUL16,
               IIC_fpALU64, IIC_fpSTAT, IIC_fpStore32, IIC_fpStore64,
               IIC_iStore_ru, IIC_fpCVTSD, IIC_fpMUL32, IIC_fpCVTIS,
               IIC_fpCVTSI, IIC_fpMOVDI, IIC_iMOVr, IIC_fpUNA64, IIC_fpMOVIS,
               IIC_iALUsi, IIC_iALUsir, IIC_iBITsi, IIC_iLoad_bh_i, IIC_iLoad_i,
               IIC_iMOVsi, IIC_iStore_i, IIC_iCMOVsi, IIC_iCMPsi,
               IIC_iLoad_bh_iu, IIC_iStore_bh_i, IIC_iALUx, IIC_iLoad_d_i,
               IIC_iUNAr, IIC_fpMOVSI, IIC_iTSTsi, IIC_fpCMP32, IIC_fpCVTDI,
               IIC_fpUNA32] =
      [mkUsage V6_Pipe 1 1]
  | it `elem` [IIC_iALUsr, IIC_iMOVsr, IIC_iBITsr, IIC_iCMPsr, IIC_iMOVix2,
               IIC_iMUL32, IIC_iLoad_si, IIC_iCMOVix2, IIC_iLoad_bh_si,
               IIC_iStore_si, IIC_fpMUL64, IIC_iMAC32, IIC_iStore_bh_si,
               IIC_iTSTsr] =
      [mkUsage V6_Pipe 1 2]
  | it `elem` [IIC_iStore_mu, IIC_iStore_m, IIC_iLoad_mu, IIC_iLoad_m,
               IIC_fpStore_mu, IIC_fpLoad_mu, IIC_iPop, IIC_fpLoad_m,
               IIC_fpStore_m, IIC_iMUL64] =
      [mkUsage V6_Pipe 1 3]
  | it `elem` [IIC_iLoad_mBr, IIC_iPop_Br] =
      [mkUsage V6_Pipe 1 4]
  | it `elem` [IIC_fpDIV32] =
      [mkUsage V6_Pipe 1 15]
  | it `elem` [IIC_fpSQRT64, IIC_fpDIV64] =
      [mkUsage V6_Pipe 1 29]

itineraryUsage _ it = error ("unmatched: itineraryUsage " ++ show it)

size i
  | i `elem` [T2MOVi32imm, T2MOVi32imm_remat] = size T2MOVi16 + size T2MOVTi16
size i
  | i `elem` [VMOVScc, VMOVDcc] = sum $ map size $ ccInstrs i
size i
  | i `elem` [JUMPTABLE_INSTS, Load_merge] = 0
size MEMCPY_4 = size T2LDMIA_4 + size T2STMIA_4
size i
  | isSourceInstr i || isDematInstr i = 0
size i =
  case SpecsGen.size i of
   0 -> error ("size of instruction " ++ show i ++ " is 0")
   b -> b `div` 2

concatUsages is =
  case map (\i -> itineraryUsage i (SpecsGen.itinerary i)) is of
   [[Usage r u o 0], [Usage r' u' o' 0]]
     | r == r' && u == u' -> [mkUsage r u (o + o')]
