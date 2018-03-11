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
module Unison.Target.Hexagon.HexagonRegisterClassDecl
    (HexagonRegisterClass (..)) where

data HexagonRegisterClass =
    CtrRegs |
    CtrRegs64 |
    DoubleRegs |
    IntRegs |
    IntRegsLow8 |
    ModRegs |
    PredRegs |
    VecDblRegs |
    VecDblRegs128B |
    VecPredRegs |
    VecPredRegs128B |
    VectorRegs |
    VectorRegs128B |
    F32 |
    M32 |
    M64 |
    P64 |
    P128 |
    G128 |
    G256 |
    RM32 |
    RM64 |
    Ptr_rc
    deriving (Eq, Ord, Show, Read)
