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
module Unison.Transformations.RenameTemps (renameTemps) where

import Data.List
import Data.Ord

import Unison.Base
import Unison.Util
import Unison.Predicates

renameTemps f _target =
    let f'  = renameOperands defTemporaries applyTempIdMap f
        f'' = f' {fCode = mapToOperationInBlocks sortAltTempsInOpr (fCode f')}
    in f''

sortAltTempsInOpr = mapToOperandIf isMOperand sortAltTemps

sortAltTemps p @ MOperand {altTemps = ts} = p {altTemps = sortBy compareTs ts}

compareTs t t'
    | isNullTemporary t && isNullTemporary t' = EQ
    | isNullTemporary t && isTemporary t' = LT
    | isTemporary t && isNullTemporary t' = GT
    | isTemporary t && isTemporary t' = comparing tId t t'
