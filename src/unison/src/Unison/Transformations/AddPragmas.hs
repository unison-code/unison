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
module Unison.Transformations.AddPragmas (addPragmas)
       where

import Unison.Base
import Unison.Util

addPragmas ps f @ Function {fComments = comments} _ =
  let comments' = comments ++ map toComment ps
  in f {fComments = comments'}

toComment (tool, args) = " " ++ pragmaHeader tool ++ " " ++ args
