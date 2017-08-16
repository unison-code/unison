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
module Unison.Transformations.CleanPragmas (cleanPragmas)
       where

import Data.Maybe

import Unison.Base
import Unison.Util

cleanPragmas ts f @ Function {fComments = comments} _ =
  let comments' = foldl removePragma comments ts
  in f {fComments = comments'}

removePragma comments t = mapMaybe (removePragmaInComment t) comments

removePragmaInComment tool comment =
    case splitPragmaComment comment of
      hdr : _ -> if hdr == (pragmaHeader tool) then Nothing else Just comment
      _ -> Just comment
