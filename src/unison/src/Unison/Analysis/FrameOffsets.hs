{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Algorithms to compute frame offsets.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.FrameOffsets
       (allocateObject, slotSet, frameSize, reoffset) where

import Data.List
import Data.Maybe
import qualified Data.Set as S

import Unison

allocateObject occ obj =
    let off   = fromJust $
                find (fitsInSlot (foMaybeSize obj) occ) [0, foAlignment obj ..]
        obj'  = obj  {foOffset = off}
        occ'  = S.union occ (S.fromList $ foSlots obj')
    in (occ', obj')

slotSet objs = S.fromList (concatMap foSlots objs)

fitsInSlot size occ s = none (\os -> S.member os occ) (slots s size)

slots init size = [init .. init + size - 1]

foSlots obj = slots (foOffset obj) (foMaybeSize obj)

frameSize objs =
  let occ = slotSet objs
  in if S.null occ then 0 else S.findMax occ + 1

reoffset delta fo @ FrameObject {foOffset = off} = fo {foOffset = off + delta}
