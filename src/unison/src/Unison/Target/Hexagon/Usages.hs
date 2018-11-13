module Unison.Target.Hexagon.Usages (usages) where

import Unison

import Unison.Target.Hexagon.Common
import Unison.Target.Hexagon.HexagonResourceDecl
import qualified Unison.Target.Hexagon.SpecsGen as SpecsGen
import Unison.Target.Hexagon.SpecsGen.HexagonItineraryDecl
import Unison.Target.Hexagon.SpecsGen.HexagonInstructionDecl

-- | Declares resource usages of each instruction

usages to i =
  let us  = usages' i
      us' = [mkUsage BundleWidth 1 1 | any (isUsageOf BundleWidth) us] ++
            [mkUsage BlockEnd 1 1 | any (isUsageOf BlockEnd) us]
  in if singleIssue to then us' else us

isUsageOf r Usage {resource = r'} = r == r'

usages' i
  | i `elem` [Jump_merge, Jr_merge] = [mkUsage BlockEnd 1 1]
usages' i
  | isConstantExtended i && not (isSourceInstr i || isDematInstr i) =
      mergeUsages (usages' (nonConstantExtendedInstr i))
                  [mkUsage BundleWidth 1 1]
  | i `elem` [C2_mux_tfr, C2_mux_tfr_new] =
      mergeUsages (itineraryUsage i $ SpecsGen.itinerary i) conNewValue
  | i `elem` spillInstrs || mayLoad' i || mayStore' i =
      mergeUsages (itineraryUsage i $ SpecsGen.itinerary i)
                  [mkUsage SpillCost 1 1]
  | i `elem` [L4_return_linear] =
      mergeUsages (itineraryUsage i $ SpecsGen.itinerary i) (store 2)
  | otherwise = itineraryUsage i $ SpecsGen.itinerary i

itineraryUsage i it
    | it `elem` [ALU32_2op_tc_1_SLOT0123, ALU32_2op_tc_2early_SLOT0123,
                 ALU32_3op_tc_1_SLOT0123, ALU32_3op_tc_2early_SLOT0123,
                 ALU32_ADDI_tc_1_SLOT0123, EXTENDER_tc_1_SLOT0123, PSEUDO] =
        baseUsage
    | it `elem` [ALU64_tc_1_SLOT23, ALU64_tc_2_SLOT23, ALU64_tc_3x_SLOT23,
                 J_tc_2early_SLOT23, M_tc_2_SLOT23, M_tc_3x_SLOT23,
                 S_2op_tc_1_SLOT23, S_2op_tc_2_SLOT23, S_2op_tc_2early_SLOT23,
                 S_3op_tc_1_SLOT23, S_3op_tc_2_SLOT23, S_3op_tc_2early_SLOT23,
                 CR_tc_2early_SLOT23, ALU64_tc_2early_SLOT23] = oneOfSlot23
    | it `elem` [CR_tc_2early_SLOT3, CR_tc_3x_SLOT3] = slot3
    | it `elem` [J_tc_2early_SLOT2] = slot2
    | it `elem` [LD_tc_ld_SLOT01, V2LDST_tc_ld_SLOT01, V4LDST_tc_ld_SLOT01] =
      oneOfSlot01
    | it `elem` [ST_tc_st_SLOT01, V2LDST_tc_st_SLOT01, V4LDST_tc_st_SLOT01] =
      oneOfSlot01 ++ store 1
      -- New-value stores cannot be issued with other stores, we model this by
      -- saturating the 'Store' resource.
    | it `elem` [ST_tc_st_SLOT0, V2LDST_tc_st_SLOT0, V4LDST_tc_st_SLOT0,
                 NCJ_tc_3or4stall_SLOT0] && mayStore' i =
      slot0 ++ store 2 ++ conNewValue
      -- A new-value compare and jump instruction i cannot be issued in parallel
      -- with stores as slot 0 will be occupied by i and slot 1 will be occupied
      -- by the instruction feeding i. We model this by saturating the 'Store'
      -- resource.
    | it `elem` [NCJ_tc_3or4stall_SLOT0] && (isNewValueCmpJump i) =
      slot0 ++ store 2
    | it `elem` [ST_tc_ld_SLOT0] = slot0 ++ store 1
    | it `elem` [LD_tc_ld_SLOT0,  ST_tc_3stall_SLOT0, NCJ_tc_3or4stall_SLOT0,
                 LD_tc_3or4stall_SLOT0] = slot0
      -- ENDLOOP instructions are encoded in the bits 14:15 of the preceeding
      -- instruction in the bundle
    | it `elem` [J_tc_2early_SLOT0123] = [mkUsage BlockEnd 1 1]
    | it `elem` [ALU32_SLOT0123_2] = mergeUsages baseUsage baseUsage
    | it `elem` [NoItinerary] = []

itineraryUsage _ it = error ("unmatched: itineraryUsage " ++ show it)

baseUsage   = [mkUsage BundleWidth 1 1, mkUsage Slot0123 1 1]
oneOfSlot01 = baseUsage ++ [mkUsage Slot01 1 1]
slot0       = oneOfSlot01 ++ [mkUsage Slot0 1 1]
oneOfSlot23 = baseUsage ++ [mkUsage Slot23 1 1]
slot2       = oneOfSlot23 ++ [mkUsage Slot2 1 1]
slot3       = oneOfSlot23 ++ [mkUsage Slot3 1 1]
store n     = [mkUsage Store n 1]
conNewValue = [mkUsage ConNewValue 1 1]
