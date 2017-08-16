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
module Unison.Tools.Export.SelectInstructions (selectInstructions) where

import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Target.Query

selectInstructions instructions f @ Function {fCode = code} target =
    let fcf    = fromCopy target
        fCode  = flatten code
        insts  = indexedInstructions $ instructionManager fCode
        i2inst = M.fromList [(o, op) | (IndexedInstruction o op) <- insts]
        o2inst = M.fromList
                 [(o, i2inst M.! i) | (o, i) <- zip fCode instructions]
        code'  = mapToOperationInBlocks (selectInstruction fcf o2inst) code
    in f {fCode = code'}

selectInstruction fcf o2inst o
  | isVirtual o =
      case o2inst M.! o of
        -- Handle nullified virtual operations -- e.g. null (pack)
        (General NullInstruction) ->
            mapToInstructions (const [mkNullInstruction]) o
        _ -> o
  | otherwise =
    let inst = o2inst M.! o
        o'   = mapToInstructions (const [inst]) o
    in maybeFromCopy fcf inst o'

maybeFromCopy fcf (TargetInstruction _) o | isCopy o = fcf o
maybeFromCopy _   _ o = o
