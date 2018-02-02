module Unison.Target.Mips.Common
       (unitLatency, isBarrierInstr, hiddenStackPointerInstruction,
        isDelaySlotInstr, isClobberRA, isRematerializable, isSourceInstr,
        isDematInstr, isRematInstr, sourceInstr, dematInstr, rematInstr,
        originalInstr) where

import qualified Data.Map as M

import Unison
import qualified Unison.Target.API as API
import Unison.Target.Mips.SpecsGen.MipsInstructionDecl
import Unison.Target.Mips.SpecsGen()

unitLatency to = API.isBoolOption "unit-latency" to

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

isDelaySlotInstr i = i `elem` delaySlotInstrs

delaySlotInstrs =
  [B, BC1F, BC1T, BEQ, BGEZ, BGTZ, BLEZ, BLTZ, BNE, JALRPseudo,
   PseudoIndirectBranch, PseudoReturn, RetRA]

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
