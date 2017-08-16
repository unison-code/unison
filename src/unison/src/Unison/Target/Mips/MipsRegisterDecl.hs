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
module Unison.Target.Mips.MipsRegisterDecl (MipsRegister (..)) where

data MipsRegister =
    A0 |
    A1 |
    A2 |
    A3 |
    AC0 |
    AT |
    D0 |
    D1 |
    D10 |
    D11 |
    D12 |
    D13 |
    D14 |
    D15 |
    D2 |
    D3 |
    D4 |
    D5 |
    D6 |
    D7 |
    D8 |
    D9 |
    D09 |
    DSPCCond |
    F0 |
    F1 |
    F10 |
    F11 |
    F12 |
    F13 |
    F14 |
    F15 |
    F16 |
    F17 |
    F18 |
    F19 |
    F2 |
    F20 |
    F21 |
    F22 |
    F23 |
    F24 |
    F25 |
    F26 |
    F27 |
    F28 |
    F29 |
    F3 |
    F30 |
    F31 |
    F4 |
    F5 |
    F6 |
    F7 |
    F8 |
    F9 |
    FCC0 |
    FP |
    GP |
    K0 |
    K1 |
    RA |
    S0 |
    S1 |
    S2 |
    S3 |
    S4 |
    S5 |
    S6 |
    S7 |
    SP |
    T0 |
    T1 |
    T2 |
    T3 |
    T4 |
    T5 |
    T6 |
    T7 |
    T8 |
    T9 |
    V0 |
    V1 |
    ZERO |
    ATT7 |
    T89 |
    FPRA |
    HI0 |
    LO0 |
    AC0_64 |
    DSPCarry |
    DSPEFI |
    DSPOutFlag16_19 |
    DSPOutFlag20 |
    DSPOutFlag21 |
    DSPOutFlag22 |
    DSPOutFlag23 |
    DSPPos |
    DSPSCount |
    HI0_64 |
    LO0_64 |
    MPL0 |
    MPL1 |
    MPL2 |
    P0 |
    P1 |
    P2
    deriving (Eq, Ord)
