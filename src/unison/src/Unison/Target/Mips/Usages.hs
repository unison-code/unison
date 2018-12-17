module Unison.Target.Mips.Usages (usages, latency) where

import Unison

import Unison.Target.Mips.Common
import Unison.Target.Mips.MipsResourceDecl
import qualified Unison.Target.Mips.SpecsGen as SpecsGen
import Unison.Target.Mips.SpecsGen.MipsItineraryDecl
import Unison.Target.Mips.SpecsGen.MipsInstructionDecl

-- | Declares resource usages of each instruction

usages to i =
  let it = SpecsGen.itinerary i
  in mergeUsages (itineraryUsage' to i it)
     ([mkUsage BundleWidth (size i) 1 | size i > 0] ++
      [mkUsage Issue (issue i) 1 | issue i > 0])

itineraryUsage' to i it =
  let us = itineraryUsage i it
  in if unitLatency to then
       [u {occupation = 1, offset = 0}
       | u  <- us, resource u /= LongDuration]
     else us

itineraryUsage i _
  | isDelaySlotInstr i = [mkUsage LongDuration 1 1]
itineraryUsage LoadGPDisp IIPseudo =
  [mkUsage ALU 1 2, Usage LongDuration 1 1 1]
itineraryUsage RetRA IIPseudo =
  itineraryUsage PseudoReturn (SpecsGen.itinerary PseudoReturn)
itineraryUsage NOP IIPseudo = [mkUsage ALU 1 1]
itineraryUsage i NoItinerary
  | i `elem` [LEA_ADDiu, LEA_ADDiu_remat, TEQ] = [mkUsage ALU 1 1]
itineraryUsage i IIPseudo
  | i `elem` [BuildPairF64, ExtractElementF64] = [mkUsage ALU 1 1]
itineraryUsage _ it
  | it `elem` [IIM16Alu, II_ADDIU, II_ADDU, II_AND, II_BADDU, II_SLL, II_SRA,
               II_SRL, II_ROTR, II_SLLV, II_SRAV, II_SRLV, II_ROTRV, II_CLO,
               II_CLZ, II_DADDIU, II_DADDU, II_DADD, II_DSLL, II_DSRL, II_DSRA,
               II_DSLLV, II_DSRLV, II_DSRAV, II_DSUBU, II_DSUB, II_DROTR,
               II_DROTRV, II_EXT, II_INS, II_LUI, II_MOVF, II_MOVN,
               II_MOVN_S, II_MOVN_D, II_MOVT, II_MOVZ, II_NOR, II_OR, II_POP,
               II_RDHWR, II_SUBU, II_XOR, II_ANDI, II_ORI, II_XORI, II_SB,
               II_SH, II_SW, II_SWL, II_SWR, II_SDL, II_SDR, II_SD, II_SAVE,
               II_SEQ_SNE, II_SEQI_SNEI, II_B, II_BBIT, II_BC, II_BC1F,
               II_BC1FL, II_BC1T, II_BC1TL, II_BCC, II_BCCZ, II_BCCZAL,
               II_BCCZALS, II_BCCZC, II_IndirectBranchPseudo, II_J, II_JAL,
               II_JALR, II_JALRC, II_JALRS, II_JALS, II_JR, II_JRADDIUSP,
               II_JRC, II_ReturnPseudo, II_CEIL, II_CVT, II_ABS, II_FLOOR,
               II_NEG, II_ROUND, II_TRUNC, II_SDC1, II_SWC1, II_SDXC1, II_SWXC1,
               II_SUXC1] =
      [mkUsage ALU 1 1]
  | it `elem` [II_LB, II_LBU, II_LH, II_LHU, II_LW, II_LWL, II_LWR, II_LD,
               II_LDL, II_LDR, II_RESTORE, II_C_CC_S, II_C_CC_D, II_LDC1,
               II_LWC1, II_LDXC1, II_LWXC1, II_LUXC1] =
      [mkUsage ALU 1 3, Usage LongDuration 1 2 1]
  | it `elem` [II_MOV_D, II_MOV_S, II_CFC1, II_CTC1, II_MOVF_D, II_MOVF_S,
               II_MOVT_D, II_MOVT_S, II_MOVZ_D, II_MOVZ_S, II_DMFC1, II_DMTC1,
               II_MFC1, II_MTC1, II_MFHC1, II_MTHC1] =
      [mkUsage ALU 1 2, Usage LongDuration 1 1 1]
  | it `elem` [II_ADD_D,II_ADD_S,II_SUB_D,II_SUB_S] =
      [mkUsage ALU 1 4, Usage LongDuration 1 3 1]
  | it `elem` [II_MUL_S, II_MADD_S, II_MSUB_S, II_NMADD_S, II_NMSUB_S] =
      [mkUsage ALU 1 7, Usage LongDuration 1 6 1]
  | it `elem` [II_MUL_D, II_MADD_D, II_MSUB_D, II_NMADD_D, II_NMSUB_D] =
      [mkUsage ALU 1 8, Usage LongDuration 1 7 1]
  | it `elem` [II_SQRT_D] =
      [mkUsage ALU 1 12, Usage LongDuration 1 11 1]
  | it `elem` [II_DIV_S] =
      [mkUsage ALU 1 23, Usage LongDuration 1 22 1]
  | it `elem` [II_DIV_D] =
      [mkUsage ALU 1 36, Usage LongDuration 1 35 1]
  | it `elem` [II_DMUL, II_DMULT, II_DMULTU, II_MADD, II_MADDU, II_MSUB,
               II_MSUBU, II_MUL, II_MULT, II_MULTU, II_MSUB, II_MSUBU] =
      [mkUsage IMULDIV 1 17, Usage LongDuration 1 16 1]
  | it `elem` [II_DIV, II_DIVU, II_DDIV, II_DDIVU] =
      [mkUsage IMULDIV 1 38, Usage LongDuration 1 37 1]
  | it `elem` [II_MFHI_MFLO, II_MTHI_MTLO] =
      [mkUsage IMULDIV 1 1]
  -- TODO: double-check this, missing from LLVM
  | it `elem` [II_SLTI_SLTIU, II_SLT_SLTU, II_SEB, II_WSBH] =
      [mkUsage ALU 1 1]
  | it `elem` [NoItinerary] = []

itineraryUsage i it =
  error ("unmatched: itineraryUsage " ++ show i ++ " " ++ show it)

size CLOBBER_RA = 0
size LoadGPDisp = size LUi + size ADDiu
size i
  | isSourceInstr i || isDematInstr i = 0
size i =
  case SpecsGen.size i of
   4 -> 1
   s -> error ("size of instruction " ++ show i ++ " is " ++ show s)

issue i
  | isDelaySlotInstr i = 0
  | otherwise = 1

-- latency to the corresponding (fun) operation (one cycle after the call)
latency _ CLOBBER_RA = 2
latency to i = maybeMax 0 $ map occupation (usages to i)
