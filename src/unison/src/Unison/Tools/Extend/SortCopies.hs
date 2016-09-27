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
module Unison.Tools.Extend.SortCopies (sortCopies) where

import Data.Ord
import Data.List
import Data.List.Split

import Unison

sortCopies f @ Function {fCode = code} _target =
    let sortedCode = map sortCopiesInBB code
    in f {fCode = sortedCode}

sortCopiesInBB b @ Block {bCode = code} =
  let sCode = split (dropBlanks $ whenElt (not . isCopy)) code
      code' = concatMap sortCopyList sCode
  in b {bCode = code'}

sortCopyList is
  | any (not . isCopy) is = is
  | otherwise = sortBy (comparing copySource) is
