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
module Unison.Transformations.RenameOperations (renameOperations) where

import qualified Data.Map as M
import Common.Util
import Unison.Base
import Unison.Util

renameOperations f @ Function {fCode = code} _target =
    let idMap = M.fromList (zip (map oId (flatten code)) [0..])
        code' = mapToOperationInBlocks (mapToOperationId (applyMap idMap)) code
    in f {fCode = code'}
