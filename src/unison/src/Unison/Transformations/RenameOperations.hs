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
module Unison.Transformations.RenameOperations (renameOperations) where

import qualified Data.Map as M
import Common.Util
import Unison.Base
import Unison.Util

renameOperations f @ Function {fCode = code} _target =
    let idMap = M.fromList (zip (map oId (flatten code)) [0..])
        code' = mapToOperationInBlocks (mapToOperationId (applyMap idMap)) code
    in f {fCode = code'}
