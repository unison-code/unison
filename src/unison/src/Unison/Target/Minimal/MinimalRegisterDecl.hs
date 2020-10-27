{-|
Copyright   :  Copyright (c) 2020, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.Minimal.MinimalRegisterDecl (MinimalRegister (..)) where

data MinimalRegister =
    R0 |
    R1 |
    R2 |
    R3 |
    R4 |
    R5 |
    R6 |
    R7
    deriving (Eq, Ord)
