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
module Unison.Transformations.RenameBlocks (renameBlocks) where

import qualified Data.Map as M

import Unison.Base
import Unison.Util
import Common.Util
import Unison.Constructors
import Unison.Predicates

renameBlocks f @ Function {fCode = code, fJumpTable = (k, jtes)} _target =
  let ids     = zip (map bLab code) [0..]
      idmap   = applyMap $ M.fromList ids
      code'   = map (renameBlock idmap) code
      code''  = mapToOperationInBlocks (applyMapToBRefOperands idmap) code'
      code''' = mapToOperationInBlocks (applyMapToJTBlocks idmap) code''
      jtes'   = map (renameBlockIdInTableEntry idmap) jtes
  in f {fCode = code''', fJumpTable = (k, jtes')}

renameBlock idmap b @ Block {bLab = l} = b {bLab = idmap l}

applyMapToBRefOperands = mapToOperandIf isBlockRef . replaceBlockRef

applyMapToJTBlocks idmap = mapToAttrJTBlocks (map idmap)

replaceBlockRef idmap (BlockRef l) = mkBlockRef (idmap l)

renameBlockIdInTableEntry idmap e @ JumpTableEntry {jtBlocks = bids} =
  e {jtBlocks = map idmap bids}
