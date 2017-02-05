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
