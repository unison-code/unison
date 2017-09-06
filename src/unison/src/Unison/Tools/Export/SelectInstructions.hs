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
        rif    = rematInstrs target
        fcode  = flatten code
        insts  = indexedInstructions $ instructionManager fcode
        i2inst = M.fromList [(o, op) | (IndexedInstruction o op) <- insts]
        o2inst = M.fromList
                 [(o, i2inst M.! i) | (o, i) <- zip fcode instructions]
        os2is  = M.fromList $ mapMaybe (aRematOrigin . oAs) fcode
        code'  = mapToOperationInBlocks
                 (selectInstruction fcf rif os2is fcode o2inst) code
    in f {fCode = code'}

selectInstruction fcf rif os2is fcode o2inst o
  | isVirtual o =
      case o2inst M.! o of
        -- Handle nullified virtual operations -- e.g. null (pack)
        (General NullInstruction) ->
            mapToInstructions (const [mkNullInstruction]) o
        _ -> o
  | otherwise =
    let inst = o2inst M.! o
        o'   = mapToInstructions (const [inst]) o
    in maybeImplement fcf rif os2is fcode inst o'

maybeImplement fcf rif os2is fcode (TargetInstruction i)
  o @ SingleOperation {oAs = as}
  | isCopy o =
    case aRematOrigin as of
     Just (roid, TargetInstruction roi) ->
       let ro = fromJust $ find (isId roid) fcode
       in implementRemat (ro, roi) (rif roi) i o
     Nothing -> fcf o
  | isLinear o =
    case M.lookup (oId o) os2is of
     Just (TargetInstruction roi) -> implementRemat (o, roi) (rif roi) i o
     Nothing -> o
maybeImplement _ _ _ _ _ o = o

implementRemat (ro, roi) (Just (si, di, ri)) i o
  -- rematerialization source instructions are just removed from the final code
  | i == si = mapToInstructions (const [mkNullInstruction]) o
  -- dematerialization copies are just removed from the final code
  | i == di = mapToInstructions (const [mkNullInstruction]) o
  -- rematerialization copies are replaced by their original operations
  | i == ri = mkLinear (oId o) [TargetInstruction roi] (oUses ro) (oDefs o)
  -- rematerialization origin instructions that do not need to be
  -- implemented in a special way
  | otherwise = o
