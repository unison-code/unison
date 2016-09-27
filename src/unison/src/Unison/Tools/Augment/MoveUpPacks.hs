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
module Unison.Tools.Augment.MoveUpPacks (moveUpPacks) where

import Unison

moveUpPacks f @ Function {fCode = code} _target =
    let code' = map moveUpPacksInBlock code
    in f {fCode = code'}

moveUpPacksInBlock b @ Block {bCode = code} =
    let packs = filter isPack code
        code' = foldl movePack code packs
    in b {bCode = code'}

movePack code o =
    case oUses o !! 1 of
      (MOperand {altTemps = [t]}) ->
          let d = potentialDefiner t code
          in moveOperation o after (isIdOf d) code
      _ -> code
