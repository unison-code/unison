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
module Unison.Target.ARM.ARMRegisterClassDecl (ARMRegisterClass (..)) where

data ARMRegisterClass =
    ALL |
    CCR |
    CRS |
    CS |
    CSL |
    CSH |
    FCS |
    FCRS |
    DPair |
    DPR |
    DPR_8 |
    DPR_VFP2 |
    GPR |
    GPRnopc |
    GPRPairOp |
    GPRsp |
    GPRwithAPSR |
    M128 |
    M512 |
    M32 |
    M32t |
    M64 |
    QPR |
    QQPR |
    QQQQPR |
    RGPR |
    RM32 |
    RM64 |
    SPR |
    TcGPR |
    TGPR |
    VecListDPair |
    VecListDPairAllLanes |
    VecListDPairSpaced |
    VecListDPairSpacedAllLanes |
    VecListFourD |
    VecListFourDAllLanes |
    VecListFourQ |
    VecListFourQAllLanes |
    VecListOneD |
    VecListOneDAllLanes |
    VecListThreeD |
    VecListThreeDAllLanes |
    VecListThreeQ |
    VecListThreeQAllLanes |
    Unknown |
    F32 |
    Ptr_rc
    deriving (Eq, Ord, Show, Read)
