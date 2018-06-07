{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Predicate functions for the Unison program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Predicates
       (
        -- * Block predicates
        isEntryBlock,
        isExitBlock,
        isReturnBlock,
        isSplitBlock,
        -- * SingleOperation predicates
        isId,
        isPhi,
        isNatural,
        isLinear,
        isBranch,
        isCall,
        isTailCall,
        isVirtual,
        isKill,
        isDefine,
        isCombine,
        isCombineLowOf,
        isCombineHighOf,
        isLow,
        isHigh,
        isSplit2,
        isSplit4,
        isDelimiter,
        isVirtualCopy,
        isFun,
        isFrameSetup,
        isFrameDestroy,
        isVirtualCopyOf,
        isCopy,
        isCopyOf,
        isNonVirtualCopy,
        isUseCopyOf,
        isDefCopyOf,
        isIn,
        isOut,
        isEntry,
        isReturn,
        isExit,
        isComponent,
        isBarrier,
        isBundle,
        isDerivedFromVirtualCopy,
        isRemat,
        isSplitBarrier,
        -- * Operation predicates
        isNaturalOpr,
        isLinearOpr,
        isBranchOpr,
        isCallOpr,
        isTailCallOpr,
        isVirtualOpr,
        isPhiOpr,
        isDelimiterOpr,
        isInOpr,
        isOutOpr,
        isEntryOpr,
        isReturnOpr,
        isExitOpr,
        isKillOpr,
        isDefineOpr,
        isCombineOpr,
        isLowOpr,
        isHighOpr,
        isSplit2Opr,
        isSplit4Opr,
        isVirtualCopyOpr,
        isFunOpr,
        isFrameSetupOpr,
        isFrameDestroyOpr,
        isNonVirtualCopyOpr,
        -- * Operand predicates
        isRegister,
        isTemporary,
        isPreAssigned,
        isBlockRef,
        isNullTemporary,
        isMOperand,
        isModelOperand,
        isBound,
        isNullableOperand,
        -- * Instruction predicates
        isNullInstruction,
        isVirtualInstruction,
        isBarrierInstruction,
        isTargetInstruction,
        -- * CGEdgeLabel predicates
        isCopyEdge,
        isComponentEdge,
        isLowEdge,
        isHighEdge,
        isCombineEdge,
        -- *  OGEdgeLabel predicates
        isOperandNaturalEdge,
        -- * RegisterClass predicates
        isNormalRegisterClass,
        isDefinedRegisterClass,
        isInfiniteRegisterClass,
        isAbstractRegisterClass,
        -- * OperandInfo predicates
        isTemporaryInfo
       )
       where

import Unison.Base

isRegister Register {} = True
isRegister _           = False

isTemporary Temporary {} = True
isTemporary _            = False

isPreAssigned Temporary {tReg = (Just _)} = True
isPreAssigned MOperand {operandReg = (Just _)} = True
isPreAssigned _                           = False

isBlockRef BlockRef {} = True
isBlockRef _           = False

isNullTemporary NullTemporary = True
isNullTemporary _             = False

isMOperand MOperand {} = True
isMOperand _           = False

isModelOperand Temporary {} = True
isModelOperand MOperand {}  = True
isModelOperand _            = False

isBound Bound {} = True
isBound _        = False

isNullableOperand p = isMOperand p && NullTemporary `elem` altTemps p

iSingleInst i @ SingleOperation {}  = oOpr i
iSingleInst Bundle {bundleOs = (i:_)} = oOpr i
iSingleInst _ = error ("unmatched iSingleInst")

-- | Predicates on blocks

isEntryBlock  = aEntry . bAs
isExitBlock   = aExit . bAs
isReturnBlock = aReturn . bAs
isSplitBlock  = aSplit . bAs

-- | Operation type predicates. Functions with the name structure isXInst
-- operate on the Operation type. Functions with the name structure isX
-- operate on the SingleOperation type

isNatural        = isNaturalOpr . iSingleInst
isLinear         = isLinearOpr . iSingleInst
isBranch         = isBranchOpr . iSingleInst
isCall           = isCallOpr . iSingleInst
isTailCall       = isTailCallOpr . iSingleInst
isVirtual        = isVirtualOpr . iSingleInst
isPhi            = isPhiOpr . iSingleInst
isDelimiter      = isDelimiterOpr . iSingleInst
isIn             = isInOpr . iSingleInst
isOut            = isOutOpr . iSingleInst
isEntry          = isEntryOpr . iSingleInst
isReturn         = isReturnOpr . iSingleInst
isExit           = isExitOpr . iSingleInst
isKill           = isKillOpr . iSingleInst
isDefine         = isDefineOpr . iSingleInst
isCombine        = isCombineOpr . iSingleInst
isLow            = isLowOpr . iSingleInst
isHigh           = isHighOpr . iSingleInst
isSplit2         = isSplit2Opr . iSingleInst
isSplit4         = isSplit4Opr . iSingleInst
isVirtualCopy    = isVirtualCopyOpr . iSingleInst
isFun            = isFunOpr . iSingleInst
isFrameSetup     = isFrameSetupOpr . iSingleInst
isFrameDestroy   = isFrameDestroyOpr . iSingleInst
isNonVirtualCopy = isNonVirtualCopyOpr . iSingleInst

isNaturalOpr (Natural _) = True
isNaturalOpr _           = False

isLinearOpr (Natural (Linear {})) = True
isLinearOpr _                     = False

isBranchOpr (Natural (Branch {})) = True
isBranchOpr _                     = False

isCallOpr (Natural (Call {})) = True
isCallOpr _                   = False

isTailCallOpr (Natural (TailCall {})) = True
isTailCallOpr _                       = False

isVirtualOpr (Virtual _) = True
isVirtualOpr _           = False

isPhiOpr (Virtual (Phi {})) = True
isPhiOpr _                  = False

isDelimiterOpr (Virtual (Delimiter _)) = True
isDelimiterOpr _                       = False

isInOpr (Virtual (Delimiter (In {}))) = True
isInOpr _                             = False

isOutOpr (Virtual (Delimiter (Out {}))) = True
isOutOpr _                              = False

isEntryOpr (Virtual (Delimiter (Entry {}))) = True
isEntryOpr _                                = False

isReturnOpr (Virtual (Delimiter (Return {}))) = True
isReturnOpr _                                 = False

isExitOpr (Virtual (Delimiter (Exit {}))) = True
isExitOpr _                               = False

isKillOpr (Virtual (Kill {})) = True
isKillOpr _                   = False

isDefineOpr (Virtual (Define {})) = True
isDefineOpr _                     = False

isCombineOpr (Virtual (Combine {})) = True
isCombineOpr _                      = False

isLowOpr (Virtual (Low {})) = True
isLowOpr _                  = False

isHighOpr (Virtual (High {})) = True
isHighOpr _                   = False

isSplit2Opr (Virtual (Split2 {})) = True
isSplit2Opr _                     = False

isSplit4Opr (Virtual (Split4 {})) = True
isSplit4Opr _                     = False

isVirtualCopyOpr (Virtual (VirtualCopy {})) = True
isVirtualCopyOpr _                          = False

isFunOpr (Virtual (Fun {})) = True
isFunOpr _                  = False

isFrameSetupOpr (Virtual (Frame (Setup {}))) = True
isFrameSetupOpr _                            = False

isFrameDestroyOpr (Virtual (Frame (Destroy {}))) = True
isFrameDestroyOpr _                              = False

isNonVirtualCopyOpr (Copy {}) = True
isNonVirtualCopyOpr _         = False

-- | Predicates that do not only test types

isCopy          = isCopyInst . oOpr
isAnyCopyOf s d = isAnyCopyInstOf s d . oOpr
isUseCopyOf us  = isCopyInstOf oCopyS us . oOpr
isDefCopyOf ds  = isCopyInstOf oCopyD ds . oOpr

isId id (SingleOperation {oId = id'}) = id == id'

isVirtualCopyOf s d i = isVirtualCopy i && isAnyCopyOf s d i

isCopyInst i = isVirtualCopyOpr i || isNonVirtualCopyOpr i

isCopyOf s d i = isCopy i && isAnyCopyOf s d i

isAnyCopyInstOf s d (Copy {oCopyS = s', oCopyD = d'})
         | s == s' && d == d' = True
         | otherwise          = False

isAnyCopyInstOf s d
  (Virtual (VirtualCopy {oVirtualCopyS = s', oVirtualCopyD = d'}))
         | s == s' && d == d' = True
         | otherwise          = False

isCombineLowOf :: Eq r => Operand r -> BlockOperation i r -> Bool
isCombineLowOf  = isCombUse oCombineLowU

isCombineHighOf :: Eq r => Operand r -> BlockOperation i r -> Bool
isCombineHighOf = isCombUse oCombineHighU

isCombUse :: Eq r => (VirtualOperation r -> Operand r) -> Operand r ->
             BlockOperation i r -> Bool
isCombUse f t SingleOperation {oOpr = (Virtual i @ Combine {})} = t == (f i)

-- TODO: it could also be a virtual copy
isCopyInstOf f ts i
    | not (isCopyInst i) = False
    | f i `elem` ts      = True
    | otherwise          = False

isComponent o = isLow o || isHigh o || isSplit2 o || isSplit4 o || isCombine o

isBarrier o = isFun o || isIn o || isOut o

isBundle Bundle {} = True
isBundle _ = False

isDerivedFromVirtualCopy
  SingleOperation {oAs = Attributes {aVirtualCopy = True}} = True
isDerivedFromVirtualCopy _ = False

isRemat SingleOperation {oAs = Attributes {aRemat = True}} = True
isRemat _ = False

isSplitBarrier SingleOperation {oAs = Attributes {aSplitBarrier = True}} = True
isSplitBarrier _ = False

isNullInstruction (General NullInstruction) = True
isNullInstruction _ = False

isVirtualInstruction (General VirtualInstruction) = True
isVirtualInstruction _ = False

isBarrierInstruction (General BarrierInstruction) = True
isBarrierInstruction _ = False

isTargetInstruction TargetInstruction {} = True
isTargetInstruction _ = False

-- | Predicates on CGEdgeLabel

isCopyEdge (CopyEdge _) = True
isCopyEdge _            = False

isComponentEdge e = isLowEdge e || isHighEdge e || isCombineEdge e

isLowEdge (LowEdge _) = True
isLowEdge _           = False

isHighEdge (HighEdge _) = True
isHighEdge _            = False

isCombineEdge (CombineEdge _) = True
isCombineEdge _               = False

-- | Predicates on OGEdgeLabel

isOperandNaturalEdge (OperandNaturalEdge _) = True
isOperandNaturalEdge _                      = False

-- | Predicates on RegisterClass

isNormalRegisterClass (RegisterClass _) = True
isNormalRegisterClass _                 = False

isDefinedRegisterClass (RegisterClass _)         = True
isDefinedRegisterClass (InfiniteRegisterClass _) = True
isDefinedRegisterClass _                         = False

isInfiniteRegisterClass (InfiniteRegisterClass _) = True
isInfiniteRegisterClass _                         = False

isAbstractRegisterClass (AbstractRegisterClass _) = True
isAbstractRegisterClass _                         = False

isTemporaryInfo TemporaryInfo {} = True
isTemporaryInfo _                = False
