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
module Unison.Target.ARM.ARMRegisterDecl (ARMRegister (..)) where

data ARMRegister =
    CPSR |
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
    FPSCR_NZCV |
    ITSTATE |
    LR |
    PC |
    R0 |
    R1 |
    R10 |
    R11 |
    R12 |
    R2 |
    R3 |
    R4 |
    R4_7 |
    R5 |
    R6 |
    R7 |
    R8 |
    R8_11 |
    R9 |
    S0 |
    S1 |
    S10 |
    S11 |
    S12 |
    S13 |
    S14 |
    S15 |
    S16 |
    S17 |
    S18 |
    S19 |
    S2 |
    S20 |
    S21 |
    S22 |
    S23 |
    S24 |
    S25 |
    S26 |
    S27 |
    S28 |
    S29 |
    S3 |
    S30 |
    S31 |
    S4 |
    S5 |
    S6 |
    S7 |
    S8 |
    S9 |
    SP |
    PRED |
    FPSCR |
    Q0 |
    Q1 |
    Q10 |
    Q11 |
    Q12 |
    Q13 |
    Q14 |
    Q15 |
    Q2 |
    Q3 |
    Q4 |
    Q5 |
    Q6 |
    Q7 |
    Q8 |
    Q9 |
    F0
    deriving (Eq, Ord)
