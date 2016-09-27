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
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Unison.Tools.Legalize (run) where

import Unison
import Unison.Driver
import Unison.Parser
import Unison.Target.API

import Unison.Transformations.RenameBlocks
import Unison.Transformations.RenameTemps
import Unison.Transformations.RenameMOperands
import Unison.Transformations.RenameOperations

run legalUniFile uni target =
  let f = parse target uni
      (f', _) =
        applyTransformations
        legalizerTransformations
        target f
  in emitOutput legalUniFile (show f')

legalizerTransformations :: Ord r =>
    [(Function i r -> TargetWithOptions i r rc s -> Function i r, String, Bool)]
legalizerTransformations =
    [(renameBlocks, "renameBlocks", True),
     (renameTemps, "renameTemps", True),
     (renameMOperands, "renameMOperands", True),
     (renameOperations, "renameOperations", True)]
