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
module Unison.Tools.Import.FoldCopies (foldCopies) where

import Data.List
import qualified Data.Set as S

import Unison

foldCopies f _target = fixpoint (foldVirtualCopy isFoldableVirtualCopy) f

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
