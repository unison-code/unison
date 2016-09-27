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
module Unison.Target.Hexagon.HexagonRegisterDecl
    (HexagonRegister (..)) where

data HexagonRegister =
    D0 |
    D0_1 |
    D0_3 |
    D1 |
    D10 |
    D10_11 |
    D11 |
    D12 |
    D12_13 |
    D12_15 |
    D13 |
    D14 |
    D14_15 |
    D15 |
    D2 |
    D2_3 |
    D3 |
    D4 |
    D4_5 |
    D4_7 |
    D5 |
    D6 |
    D6_7 |
    D7 |
    D8 |
    D8_11 |
    D8_9 |
    D9 |
    F0 |
    P0 |
    P0_1 |
    P0_3 |
    P1 |
    P2 |
    P2_3 |
    P3 |
    R0 |
    R1 |
    R10 |
    R11 |
    R12 |
    R13 |
    R14 |
    R15 |
    R16 |
    R17 |
    R18 |
    R19 |
    R2 |
    R20 |
    R21 |
    R22 |
    R23 |
    R24 |
    R25 |
    R26 |
    R27 |
    R28 |
    R29 |
    R3 |
    R30 |
    R31 |
    R4 |
    R5 |
    R6 |
    R7 |
    R8 |
    R9 |
    PC |
    M0 |
    M1 |
    LC0 |
    LC1 |
    SA0 |
    SA1 |
    USR |
    USR_OVF |
    CS |
    GP
    deriving (Eq, Ord)
