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
module Unison.Transformations.AddPragmas (addPragmas)
       where

import Unison.Base
import Unison.Util

addPragmas ps f @ Function {fComments = comments} _ =
  let comments' = comments ++ map toComment ps
  in f {fComments = comments'}

toComment (tool, args) = " " ++ pragmaHeader tool ++ " " ++ args
