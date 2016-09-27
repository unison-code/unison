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
module Unison.Tools.Import.FoldCopies (foldCopies) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Unison

instance Eq i => Eq (Function i r)
  where f == f' = fCode f == fCode f'

instance Eq i => Eq (Block i r)
  where b == b' = bCode b == bCode b'

foldCopies f _target = fixpoint removeCopy f

removeCopy f @ Function {fCode = code}
    | none (isFoldableVirtualCopy code) (flatten code) = f
removeCopy f @ Function {fCode = code, fCongruences = cs} =
    let (d, s) = findCopy code
        code'  = filterCode (not . isCopyOf s d) code
        d2s    = M.fromList [(tId d, tId s)]
        code'' = mapToOperationInBlocks (mapToModelOperand (applyTempIdMap d2s)) code'
        cs'    = map (mapTuple (applyTempIdMap d2s)) cs
    in f {fCode = code'', fCongruences = cs'}

findCopy code =
  let fcode       = flatten code
      (Just copy) = find (isFoldableVirtualCopy code) fcode
      (s, d)      = copyOps copy
  in (d, s)

isFoldableVirtualCopy code o = isVirtualCopy o && isFoldable code o

-- A copy is foldable iff: a) the source and the destination are not used by the
-- same operation (e.g. a function call with different pre-assignments) and b)
-- the source is not defined by a phi operation such that the destination is
-- live-out of its block.
isFoldable code o =
  let fcode  = flatten code
      (s, d) = (copySource o, copyDestination o)
  in (null $ intersection (users s fcode) (users d fcode)) &&
     not ((isPhi $ definer s fcode) && liveOut code d)

intersection a b = S.toList $ S.intersection (S.fromList a) (S.fromList b)

liveOut code t =
  let bcode = tempBlock code t
      code' = code \\ [bcode]
  in any (\o -> isPhi o && isUser t o) (bCode bcode) ||
     (not (null (users t (flatten code'))))
