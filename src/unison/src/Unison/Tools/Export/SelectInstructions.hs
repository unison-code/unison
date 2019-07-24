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

data RematType = Source | Demat | Remat deriving (Eq, Ord, Show)

selectInstructions instructions f @ Function {fCode = code} target =
    let fcf    = fromCopy target
        rif    = rematInstrs target
        fcode  = flatten code
        i2rt   = rematTypeMap rif fcode
        insts  = indexedInstructions $ instructionManager fcode
        i2inst = M.fromList [(o, op) | (IndexedInstruction o op) <- insts]
        o2inst = M.fromList
                 [(o, i2inst M.! i) | (o, i) <- zip fcode instructions]
        code'  = mapToOperationInBlocks
                 (selectInstruction fcf i2rt fcode o2inst) code
    in f {fCode = code'}

selectInstruction fcf i2rt fcode o2inst o
  | isVirtual o =
      case o2inst M.! o of
        -- Handle nullified virtual operations -- e.g. null (pack)
        (General NullInstruction) ->
            mapToInstructions (const [mkNullInstruction]) o
        _ -> o
  | otherwise =
    let inst = o2inst M.! o
        o'   = mapToInstructions (const [inst]) o
    in maybeImplement fcf i2rt fcode inst o'

maybeImplement fcf i2rt fcode (TargetInstruction i)
  o @ SingleOperation {oAs = as}
  | isCopy o =
    case M.lookup i i2rt of
     -- Demat instructions are just removed from the final code.
     Just (Demat, _) ->
       o {oOpr = Natural Linear {oIs = [mkNullInstruction],
                                 oUs = [oCopyS $ oOpr o],
                                 oDs = [oCopyD $ oOpr o]}}
     -- Remat instructions are replaced by clones of their original operations
     -- (except the definition operand which is preserved).
     Just (Remat, orig) ->
       let roid = fromJust $ aRematOrigin as
           ro   = fromJust $ find (isId roid) fcode
       in o {oOpr = Natural Linear {oIs = [TargetInstruction orig],
                                    oUs = oUses ro,
                                    oDs = [oCopyD $ oOpr o]}}
     -- Regular copies are implemented in a target-dependent fashion.
     Nothing -> fcf o
  | isLinear o =
    case M.lookup i i2rt of
    -- Source instructions are just removed from the final code.
    Just (Source, _) -> mapToInstructions (const [mkNullInstruction]) o
    Nothing -> o
maybeImplement _ _ _ _ o = o

-- Build a map from instructions to maybe their remat type ('source', 'demat',
-- or 'remat').
rematTypeMap rif fcode =
  let ros  = [fromJust $ find (isId oid) fcode
             | oid <- mapMaybe rematOrigin fcode]
      ris  = nub $ [oTargetInstr gi | gi <- concatMap oInstructions ros,
                   isTargetInstruction gi]
      i2rt = M.fromList $ concat [rematType i (rif i) | i <- ris]
  in i2rt

rematOrigin SingleOperation {oAs = Attributes {aRematOrigin = ro}} = ro

rematType _ Nothing = []
rematType i (Just (si, di, ri)) =
  [(si, (Source, i)), (di, (Demat, i)), (ri, (Remat, i))]
