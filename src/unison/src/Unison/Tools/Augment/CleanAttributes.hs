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
module Unison.Tools.Augment.CleanAttributes (cleanAttributes) where

import qualified Data.Set as S

import Unison

cleanAttributes f @ Function {fCode = code} _target =
    let insts = S.fromList $ concatMap oInstructions (flatten code)
        f'    = mapToOperation (cleanActivators insts) f
    in f'

cleanActivators insts = mapToActivators (intersectionWithList insts)

intersectionWithList set = S.toList . S.intersection set . S.fromList
