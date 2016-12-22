module Unison.Target.Mips.Common
       (isBarrierInstr, hiddenStackPointerInstruction, isDelaySlotInstr) where

import qualified Data.Map as M

import Unison.Target.Mips.SpecsGen.MipsInstructionDecl
import Unison.Target.Mips.SpecsGen()

isBarrierInstr i = i `elem` barrierInstrs

barrierInstrs = [LoadGPDisp]

hiddenStackPointerInstruction i =
  case M.lookup i hiddenStackPointerVersions of
      (Just i') -> i'
      Nothing -> error $ "unmatched: hiddenStackPointerVersions " ++ show i

hiddenStackPointerVersions = M.fromList
  [(SW, SW_sp),
   (SWC1, SWC1_sp)]

isDelaySlotInstr i = i `elem` delaySlotInstrs

delaySlotInstrs =
  [B, BC1F, BC1T, BEQ, BGEZ, BGTZ, BLEZ, BLTZ, BNE, JALRPseudo,
   PseudoIndirectBranch, PseudoReturn, RetRA]
