{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Main (main) where

import SpecsGen.Driver

main = runSpecsGen id (\_ _ -> return ())
