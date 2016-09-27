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
    G256
    deriving (Eq, Ord, Show, Read)
