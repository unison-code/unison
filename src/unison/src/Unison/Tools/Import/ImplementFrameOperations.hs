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
module Unison.Tools.Import.ImplementFrameOperations (implementFrameOperations) where

import Unison
import Unison.Target.API

implementFrameOperations implementFrames f @ Function {fCode = code} target =
    let iff   = implementFrame target
        code' = map (implementFrameOperationsInBlock implementFrames iff) code
    in f {fCode = code'}

implementFrameOperationsInBlock implementFrames iff b @ Block {bCode = code} =
    let code' = concatMap (implementFrameOperation implementFrames iff) code
    in b {bCode = code'}

implementFrameOperation implementFrames iff o
    | implementFrames && (isFrameSetup o || isFrameDestroy o) = iff o
    | otherwise = [o]
