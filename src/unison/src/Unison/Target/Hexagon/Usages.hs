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
    | it `elem` [Tc_002cb246] = oneOfSlot23
    | it `elem` [Tc_0371abea] = oneOfSlot01
    | it `elem` [Tc_05c070ec] = baseUsage
    | it `elem` [Tc_05d3a09b] = oneOfSlot23
    | it `elem` [Tc_0663f615] = oneOfSlot23
    | it `elem` [Tc_096199d3] = slot0
    | it `elem` [Tc_0a705168] = oneOfSlot01
    | it `elem` [Tc_0ae0825c] = oneOfSlot23
    | it `elem` [Tc_0b2be201] = oneOfSlot01
    | it `elem` [Tc_0d8f5752] = slot3
    | it `elem` [Tc_13bfbcf9] = oneOfSlot23
    | it `elem` [Tc_14b272fa] = oneOfSlot01
    | it `elem` [Tc_14b5c689] = oneOfSlot23
    | it `elem` [Tc_15aa71c5] = oneOfSlot01
    | it `elem` [Tc_174516e8] = slot3
    | it `elem` [Tc_17e0d2cd] = oneOfSlot01
    | it `elem` [Tc_1a2fd869] = oneOfSlot23
    | it `elem` [Tc_1ad90acd] = slot2
    | it `elem` [Tc_1ae57e39] = baseUsage
    | it `elem` [Tc_1b6f7cec] = baseUsage
    | it `elem` [Tc_1c4528a2] = slot3
    | it `elem` [Tc_1c80410a] = baseUsage
    | it `elem` [Tc_1d81e60e] = oneOfSlot23
    | it `elem` [Tc_1fc97744] = oneOfSlot23
    | it `elem` [Tc_20cdee80] = oneOfSlot23
    | it `elem` [Tc_2332b92e] = oneOfSlot23
    | it `elem` [Tc_24b66c99] = oneOfSlot01
    | it `elem` [Tc_25a78932] = oneOfSlot01
    | it `elem` [Tc_2b8da4c2] = slot0
    | it `elem` [Tc_2eabeebe] = baseUsage
    | it `elem` [Tc_2f7c551d] = oneOfSlot23
    | it `elem` [Tc_2ff964b4] = oneOfSlot23
    | it `elem` [Tc_30b9bb4a] = oneOfSlot01
    | it `elem` [Tc_32779c6f] = slot3
    | it `elem` [Tc_36153880] = slot0
    | it `elem` [Tc_362c6592] = oneOfSlot01
    | it `elem` [Tc_3962fa26] = oneOfSlot01
    | it `elem` [Tc_39dfefe8] = oneOfSlot01
    | it `elem` [Tc_3a867367] = oneOfSlot23
    | it `elem` [Tc_3b470976] = oneOfSlot23
    | it `elem` [Tc_3b5b7ef9] = oneOfSlot01
    | it `elem` [Tc_3bd75825] = slot2
    | it `elem` [Tc_3c76b0ff] = oneOfSlot01
    | it `elem` [Tc_3d495a39] = oneOfSlot23
    | it `elem` [Tc_40116ca8] = oneOfSlot01
    | it `elem` [Tc_434c8e1e] = slot3
    | it `elem` [Tc_4414d8b1] = oneOfSlot23
    | it `elem` [Tc_44d3da28] = oneOfSlot01
    | it `elem` [Tc_4560740b] = oneOfSlot23
    | it `elem` [Tc_4837eefb] = oneOfSlot23
    | it `elem` [Tc_49a8207d] = slot0
    | it `elem` [Tc_4ae7b58b] = oneOfSlot23
    | it `elem` [Tc_4b68bce4] = slot0
    | it `elem` [Tc_4c5ba658] = baseUsage
    | it `elem` [Tc_4d5fa3a1] = slot0
    | it `elem` [Tc_53559e35] = oneOfSlot01
    | it `elem` [Tc_56336eb0] = oneOfSlot23
    | it `elem` [Tc_56f114f4] = baseUsage
    | it `elem` [Tc_57890846] = baseUsage
    | it `elem` [Tc_5a2711e5] = baseUsage
    | it `elem` [Tc_5abb5e3f] = slot0
    | it `elem` [Tc_5aee39f7] = oneOfSlot01
    | it `elem` [Tc_5b54b33f] = oneOfSlot23
    | it `elem` [Tc_5b7c0967] = oneOfSlot01
    | it `elem` [Tc_5bf126a6] = slot0
    | it `elem` [Tc_5d7f5414] = slot2
    | it `elem` [Tc_5ef37dc4] = oneOfSlot01
    | it `elem` [Tc_6132ba3d] = oneOfSlot23
    | it `elem` [Tc_61830035] = baseUsage
    | it `elem` [Tc_640086b5] = oneOfSlot23
    | it `elem` [Tc_643b4717] = oneOfSlot23
    | it `elem` [Tc_67435e81] = slot0
    | it `elem` [Tc_675e4897] = slot0
    | it `elem` [Tc_679309b8] = oneOfSlot23
    | it `elem` [Tc_6b25e783] = slot3
    | it `elem` [Tc_703e822c] = oneOfSlot23
    | it `elem` [Tc_7186d325] = slot0
    | it `elem` [Tc_7646c131] = oneOfSlot01
    | it `elem` [Tc_76851da1] = oneOfSlot23
    | it `elem` [Tc_779080bf] = oneOfSlot23
    | it `elem` [Tc_784490da] = oneOfSlot23
    | it `elem` [Tc_785f65a7] = oneOfSlot01
    | it `elem` [Tc_7a91e76a] = oneOfSlot01
    | it `elem` [Tc_838b34ea] = slot0
    | it `elem` [Tc_85c9c08f] = slot2
    | it `elem` [Tc_85d5d03f] = oneOfSlot23
    | it `elem` [Tc_862b3e70] = baseUsage
    | it `elem` [Tc_88b4f13d] = oneOfSlot23
    | it `elem` [Tc_89e94ad3] = oneOfSlot01
    | it `elem` [Tc_8b121f4a] = slot2
    | it `elem` [Tc_8b3e402a] = oneOfSlot01
    | it `elem` [Tc_8c945be0] = slot0
    | it `elem` [Tc_8c99de45] = slot0
    | it `elem` [Tc_8d9d0154] = slot2
    | it `elem` [Tc_8fb7ab1b] = slot0
    | it `elem` [Tc_9461ff31] = oneOfSlot23
    | it `elem` [Tc_946df596] = oneOfSlot23
    | it `elem` [Tc_9ad9998f] = slot3
    | it `elem` [Tc_9bfd761f] = slot0
    | it `elem` [Tc_9c3ecd83] = oneOfSlot23
    | it `elem` [Tc_9ca930f7] = slot0
    | it `elem` [Tc_9da59d12] = slot0
    | it `elem` [Tc_9debc299] = oneOfSlot23
    | it `elem` [Tc_9e313203] = oneOfSlot23
    | it `elem` [Tc_9fc3dae0] = oneOfSlot01
    | it `elem` [Tc_a1123dda] = oneOfSlot01
    | it `elem` [Tc_a1c00888] = oneOfSlot23
    | it `elem` [Tc_a58fd5cc] = oneOfSlot23
    | it `elem` [Tc_a5d4aeec] = oneOfSlot01
    | it `elem` [Tc_a6b1eca9] = oneOfSlot01
    | it `elem` [Tc_a813cf9a] = slot3
    | it `elem` [Tc_a9d88b22] = slot3
    | it `elem` [Tc_ae53734a] = oneOfSlot23
    | it `elem` [Tc_b31c2e97] = oneOfSlot23
    | it `elem` [Tc_b343892a] = slot0
    | it `elem` [Tc_b43e7930] = slot0
    | it `elem` [Tc_b4407292] = slot0
    | it `elem` [Tc_b44ecf75] = slot0
    | it `elem` [Tc_b4b5c03a] = oneOfSlot23
    | it `elem` [Tc_b51dc29a] = slot2
    | it `elem` [Tc_b83e6d73] = oneOfSlot01
    | it `elem` [Tc_b857bf4e] = slot0
    | it `elem` [Tc_b8bffe55] = oneOfSlot23
    | it `elem` [Tc_b90a29b1] = slot0
    | it `elem` [Tc_b9272d6c] = slot3
    | it `elem` [Tc_b9e09e03] = slot2
    | it `elem` [Tc_bab0eed9] = oneOfSlot01
    | it `elem` [Tc_bafaade3] = oneOfSlot23
    | it `elem` [Tc_bcf98408] = slot3
    | it `elem` [Tc_bd8382d1] = slot0
    | it `elem` [Tc_bdceeac1] = oneOfSlot23
    | it `elem` [Tc_be9602ff] = slot0
    | it `elem` [Tc_bf061958] = oneOfSlot01
    | it `elem` [Tc_bfec0f01] = oneOfSlot23
    | it `elem` [Tc_c4db48cb] = oneOfSlot01
    | it `elem` [Tc_c4f596e3] = oneOfSlot01
    | it `elem` [Tc_c79a189f] = slot0
    | it `elem` [Tc_c8ce0b5c] = oneOfSlot23
    | it `elem` [Tc_cd374165] = oneOfSlot23
    | it `elem` [Tc_cf8126ae] = oneOfSlot23
    | it `elem` [Tc_cfd8378a] = oneOfSlot23
    | it `elem` [Tc_d08ee0f4] = baseUsage
    | it `elem` [Tc_d1aa9eaa] = oneOfSlot23
    | it `elem` [Tc_d2e63d61] = slot0
    | it `elem` [Tc_d5b7b0c1] = slot2
    | it `elem` [Tc_d5c0729a] = slot0
    | it `elem` [Tc_d63f638c] = slot0
    | it `elem` [Tc_d65dbf51] = slot0
    | it `elem` [Tc_d773585a] = oneOfSlot23
    | it `elem` [Tc_d9d43ecb] = slot3
    | it `elem` [Tc_da4a37ed] = oneOfSlot01
    | it `elem` [Tc_da97ee82] = oneOfSlot01
    | it `elem` [Tc_db2bce9c] = oneOfSlot23
    | it `elem` [Tc_de4df740] = baseUsage
    | it `elem` [Tc_de554571] = oneOfSlot23
    | it `elem` [Tc_df3319ed] = slot3
    | it `elem` [Tc_e06f432a] = slot0
    | it `elem` [Tc_e4a7f9f0] = oneOfSlot23
    | it `elem` [Tc_e4b3cb20] = oneOfSlot01
    | it `elem` [Tc_e78647bd] = slot2
    | it `elem` [Tc_e86aa961] = oneOfSlot01
    | it `elem` [Tc_e93a3d71] = oneOfSlot01
    | it `elem` [Tc_e95795ec] = slot0
    | it `elem` [Tc_e9f3243f] = oneOfSlot01
    | it `elem` [Tc_f429765c] = oneOfSlot23
    | it `elem` [Tc_f675fee8] = oneOfSlot23
    | it `elem` [Tc_f8e23f0b] = oneOfSlot01
    | it `elem` [Tc_f9058dd7] = oneOfSlot23
    | it `elem` [Tc_fc3999b4] = slot2
    | it `elem` [Tc_fcc3ddf9] = slot0
    | it `elem` [Tc_fe211424] = slot0
    | it `elem` [LD_tc_ld_SLOT01] = oneOfSlot01
    | it `elem` [ST_tc_st_SLOT01] = oneOfSlot01 ++ store 1
      -- New-value stores cannot be issued with other stores, we model this by
      -- saturating the 'Store' resource.
    | it `elem` [ST_tc_st_SLOT0, NCJ_tc_3or4stall_SLOT0] && mayStore' i =
      slot0 ++ store 2 ++ conNewValue
      -- A new-value compare and jump instruction i cannot be issued in parallel
      -- with stores as slot 0 will be occupied by i and slot 1 will be occupied
      -- by the instruction feeding i. We model this by saturating the 'Store'
      -- resource.
    | it `elem` [NCJ_tc_3or4stall_SLOT0] && (isNewValueCmpJump i) =
      slot0 ++ store 2
    -- ENDLOOP instructions are encoded in the bits 14:15 of the preceding
    -- instruction in the bundle
    | it `elem` [Tc_ENDLOOP] = [mkUsage BlockEnd 1 1]
    | it `elem` [NoItinerary] = []
    -- | it `elem` [ALU32_2op_tc_1_SLOT0123, ALU32_2op_tc_2early_SLOT0123,
    --              ALU32_3op_tc_1_SLOT0123, ALU32_3op_tc_2early_SLOT0123,
    --              ALU32_ADDI_tc_1_SLOT0123, EXTENDER_tc_1_SLOT0123, PSEUDO] =
    --     baseUsage
    -- | it `elem` [ALU64_tc_1_SLOT23, ALU64_tc_2_SLOT23, ALU64_tc_3x_SLOT23,
    --              J_tc_2early_SLOT23, M_tc_2_SLOT23, M_tc_3x_SLOT23,
    --              S_2op_tc_1_SLOT23, S_2op_tc_2_SLOT23, S_2op_tc_2early_SLOT23,
    --              S_3op_tc_1_SLOT23, S_3op_tc_2_SLOT23, S_3op_tc_2early_SLOT23,
    --              CR_tc_2early_SLOT23, ALU64_tc_2early_SLOT23] = oneOfSlot23
    -- | it `elem` [CR_tc_2early_SLOT3, CR_tc_3x_SLOT3] = slot3
    -- | it `elem` [J_tc_2early_SLOT2] = slot2
    -- | it `elem` [LD_tc_ld_SLOT01, V2LDST_tc_ld_SLOT01, V4LDST_tc_ld_SLOT01] =
    --   oneOfSlot01
    -- | it `elem` [ST_tc_st_SLOT01, V2LDST_tc_st_SLOT01, V4LDST_tc_st_SLOT01] =
    --   oneOfSlot01 ++ store 1
    --   -- New-value stores cannot be issued with other stores, we model this by
    --   -- saturating the 'Store' resource.
    -- | it `elem` [ST_tc_st_SLOT0, V2LDST_tc_st_SLOT0, V4LDST_tc_st_SLOT0,
    --              NCJ_tc_3or4stall_SLOT0] && mayStore' i =
    --   slot0 ++ store 2 ++ conNewValue
    --   -- A new-value compare and jump instruction i cannot be issued in parallel
    --   -- with stores as slot 0 will be occupied by i and slot 1 will be occupied
    --   -- by the instruction feeding i. We model this by saturating the 'Store'
    --   -- resource.
    -- | it `elem` [NCJ_tc_3or4stall_SLOT0] && (isNewValueCmpJump i) =
    --   slot0 ++ store 2
    -- | it `elem` [ST_tc_ld_SLOT0] = slot0 ++ store 1
    -- | it `elem` [LD_tc_ld_SLOT0,  ST_tc_3stall_SLOT0, NCJ_tc_3or4stall_SLOT0,
    --              LD_tc_3or4stall_SLOT0] = slot0
    --   -- ENDLOOP instructions are encoded in the bits 14:15 of the preceeding
    --   -- instruction in the bundle
    -- | it `elem` [J_tc_2early_SLOT0123] = [mkUsage BlockEnd 1 1]
    -- | it `elem` [ALU32_SLOT0123_2] = mergeUsages baseUsage baseUsage
    -- | it `elem` [NoItinerary] = []

itineraryUsage _ it = error ("unmatched: itineraryUsage " ++ show it)

baseUsage   = [mkUsage BundleWidth 1 1, mkUsage Slot0123 1 1]
oneOfSlot01 = baseUsage ++ [mkUsage Slot01 1 1]
slot0       = oneOfSlot01 ++ [mkUsage Slot0 1 1]
oneOfSlot23 = baseUsage ++ [mkUsage Slot23 1 1]
slot2       = oneOfSlot23 ++ [mkUsage Slot2 1 1]
slot3       = oneOfSlot23 ++ [mkUsage Slot3 1 1]
store n     = [mkUsage Store n 1]
conNewValue = [mkUsage ConNewValue 1 1]
