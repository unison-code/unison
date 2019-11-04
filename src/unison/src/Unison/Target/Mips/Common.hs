module Unison.Target.Mips.Common
       (unitLatency, noDelaySlots, keepNops, isBarrierInstr,
        hiddenStackPointerInstruction, isDelaySlotInstr, isDelaySlotNOPInstr,
        delaySlotInstr, delaySlotNOPInstr, addDelaySlotNOPInstr, isClobberRA,
        isRematerializable, isSourceInstr, isDematInstr, isRematInstr,
        sourceInstr, dematInstr, rematInstr, originalInstr) where

import qualified Data.Map as M

import Unison
import qualified Unison.Target.API as API
import Unison.Target.Mips.SpecsGen.MipsInstructionDecl
import Unison.Target.Mips.SpecsGen()

unitLatency to = API.isBoolOption "unit-latency" to

noDelaySlots to = API.isBoolOption "no-delay-slots" to

keepNops to = API.isBoolOption "keep-nops" to

isBarrierInstr i = i `elem` barrierInstrs

barrierInstrs = [LoadGPDisp]

hiddenStackPointerInstruction i =
  case M.lookup i hiddenStackPointerVersions of
      (Just i') -> i'
      Nothing -> error $ "unmatched: hiddenStackPointerVersions " ++ show i

hiddenStackPointerVersions = M.fromList
  [(SW, SW_sp),
   (SWC1, SWC1_sp),
   (SDC1, SDC1_sp)]

isDelaySlotInstr i = M.member i delaySlotNOPVersions
isDelaySlotNOPInstr i = M.member i (inverseMap delaySlotNOPVersions)
delaySlotInstr i = (inverseMap delaySlotNOPVersions) M.! i
delaySlotNOPInstr i = delaySlotNOPVersions M.! i

delaySlotNOPVersions = M.fromList
  [(B, B_NOP), (BC1F, BC1F_NOP), (BC1T, BC1T_NOP), (BEQ, BEQ_NOP),
   (BGEZ, BGEZ_NOP), (BGTZ, BGTZ_NOP), (BLEZ, BLEZ_NOP), (BLTZ, BLTZ_NOP),
   (BNE, BNE_NOP), (JALRPseudo, JALRPseudo_NOP),
   (PseudoIndirectBranch, PseudoIndirectBranch_NOP),
   (PseudoReturn, PseudoReturn_NOP), (RetRA, RetRA_NOP)]

addDelaySlotNOPInstr i
  | isDelaySlotInstr i = [i, delaySlotNOPInstr i]
  | otherwise = [i]

isClobberRA o =
  isNatural o && (TargetInstruction CLOBBER_RA) `elem` oInstructions o

data RematTriple = RematTriple {
  source :: MipsInstruction,
  demat  :: MipsInstruction,
  remat  :: MipsInstruction}

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

rematVersions :: M.Map MipsInstruction RematTriple
rematVersions = M.fromList
  [(LDC1_fi, RematTriple LDC1_fi_source_fi LDC1_fi_demat_fi LDC1_fi_remat_fi),
   (LEA_ADDiu, RematTriple LEA_ADDiu_source LEA_ADDiu_demat LEA_ADDiu_remat),
   (LUi, RematTriple LUi_source LUi_demat LUi_remat),
   (LW_fi, RematTriple LW_fi_source_fi LW_fi_demat_fi LW_fi_remat_fi),
   (LBu_fi, RematTriple LBu_fi_demat_fi LBu_fi_source_fi LBu_fi_remat_fi),
   (LWC1_fi, RematTriple LWC1_fi_demat_fi LWC1_fi_source_fi LWC1_fi_remat_fi)]
