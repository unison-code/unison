{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Transformations.RenameOperations (renameOperations) where

import qualified Data.Map as M
import Control.Arrow

import Common.Util
import Unison.Base
import Unison.Util

renameOperations f @ Function {fCode = code, fRematerializable = rts} _target =
    let idMap = M.fromList (zip (map oId (flatten code)) [0..])
        code' = mapToOperationInBlocks (mapToOperationId (applyMap idMap)) code
        rts'  = map (second (map (applyMap idMap))) rts
    in f {fCode = code', fRematerializable = rts'}
