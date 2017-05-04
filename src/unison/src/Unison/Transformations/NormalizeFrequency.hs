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
module Unison.Transformations.NormalizeFrequency (normalizeFrequency)
       where

import Unison.Base
import Unison.Util

normalizeFrequency f @ Function {fCode = code, fRemovedFreqs = rfs} _ =
  let rawfreq = map blockFreq code ++ rfs
      freq    = normalize rawfreq
      code'   = map updateBlockFreq (zip freq code)
  in f {fCode = code'}
