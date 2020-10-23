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
module Unison.Tools.Export.BundleOperations (bundleOperations) where

import qualified Data.Map as M
import qualified Data.Set as S

import Unison

bundleOperations cycles f @ Function {fCode = code} _ =
    let i2c   = M.fromList (zip (flatten code) cycles)
        bcode = map (toBundleBlock i2c) code
    in f {fCode = bcode}

toBundleBlock i2c b @ Block {bCode = code} =
  let code' = filter isActive code
      is    = S.fromList code'
      i2c'  = M.filterWithKey (\i _ -> S.member i is) i2c
      c2is  = foldr mapAppend M.empty [(c, [i]) | (i, c) <- M.toList i2c']
      cs    = zip [0 .. fst (M.findMax c2is)] (repeat [])
      c2is' = M.toList $ foldr mapAppend c2is cs
      bcode = map (Bundle . snd) c2is'
  in b {bCode = bcode}

isActive o | oInstructions o == [mkNullInstruction] = False
isActive _ = True