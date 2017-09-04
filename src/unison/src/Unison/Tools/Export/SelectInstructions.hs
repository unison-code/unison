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

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Unison
import Unison.Target.API
import Unison.Target.Query

selectInstructions instructions f @ Function {fCode = code} target =
    let fcf    = fromCopy target
        rcf    = rematCopies target
        fcode  = flatten code
        insts  = indexedInstructions $ instructionManager fcode
        i2inst = M.fromList [(o, op) | (IndexedInstruction o op) <- insts]
        o2inst = M.fromList
                 [(o, i2inst M.! i) | (o, i) <- zip fcode instructions]
        code'  = mapToOperationInBlocks (selectInstruction fcf rcf fcode o2inst)
                 code
    in f {fCode = code'}

selectInstruction fcf rcf fcode o2inst o
  | isVirtual o =
      case o2inst M.! o of
        -- Handle nullified virtual operations -- e.g. null (pack)
        (General NullInstruction) ->
            mapToInstructions (const [mkNullInstruction]) o
        _ -> o
  | otherwise =
    let inst = o2inst M.! o
        o'   = mapToInstructions (const [inst]) o
    in maybeFromCopy fcf rcf fcode inst o'

maybeFromCopy fcf rcf fcode (TargetInstruction i) o @ SingleOperation {oAs = as}
  | isCopy o =
    case aRematOrigin as of
     Just roid ->
       let ro = fromJust $ find (isId roid) fcode
       in implementRemat ro (rcf (targetInst (oInstructions ro))) i o
     Nothing -> fcf o
maybeFromCopy _ _ _  _ o = o

implementRemat ro (Just (di, ri)) i o
  -- dematerialization copies are just removed from the final code
  | i == di = mapToInstructions (const [mkNullInstruction]) o
  -- rematerialization copies are replaced by their original operations
  | i == ri = mkLinear (oId o) (oInstructions ro) (oUses ro) (oDefs o)
