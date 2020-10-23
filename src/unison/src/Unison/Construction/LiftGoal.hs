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
module Unison.Construction.LiftGoal (liftGoal) where

import Data.List.Split

import Unison.Base
import Unison.Instances()

liftGoal maybeGoal f _ =
  let hlgoal = case maybeGoal of
                Nothing   -> []
                Just goal -> map read (splitOn "," goal)
  in f {fGoal = hlgoal}
