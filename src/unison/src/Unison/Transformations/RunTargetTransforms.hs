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
module Unison.Transformations.RunTargetTransforms (runTargetTransforms) where

import Unison.Target.API

runTargetTransforms phase f target =
    let tfs = transforms target phase
        f'  = foldl applyTransform f tfs
    in f'

applyTransform f tf = tf f
