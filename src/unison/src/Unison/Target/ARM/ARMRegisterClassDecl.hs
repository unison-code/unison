{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.ARM.ARMRegisterClassDecl (ARMRegisterClass (..)) where

data ARMRegisterClass =
    ALL |
    CCR |
    CS |
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
    M32 |
    M32t |
    M64 |
    QPR |
    QQPR |
    QQQQPR |
    RGPR |
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
    Unknown
    deriving (Eq, Ord, Show, Read)
