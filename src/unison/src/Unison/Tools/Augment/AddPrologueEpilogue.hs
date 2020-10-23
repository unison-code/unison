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
module Unison.Tools.Augment.AddPrologueEpilogue (addPrologueEpilogue) where

import Unison
import Unison.Target.API

addPrologueEpilogue f @ Function {fCode = code} target =
    let apf    = addPrologue target
        aef    = addEpilogue target
        ids    = newIndexes $ flatten code
        code'  = mapToEntryBlock (apf ids) code
        outBs  = returnBlockIds code'
        code'' = foldl (addEpilogueInBlock aef) code' outBs
    in f {fCode = code''}

addEpilogueInBlock aef code l =
    let ids   = newIndexes $ flatten code
        code' = mapToBlock (aef ids) l code
    in code'
