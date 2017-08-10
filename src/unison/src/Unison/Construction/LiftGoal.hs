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
module Unison.Construction.LiftGoal (liftGoal) where

import Data.List.Split

import Unison.Base
import Unison.Instances()

liftGoal maybeGoal f _ =
  let hlgoal = case maybeGoal of
                Nothing   -> []
                Just goal -> map read (splitOn "," goal)
  in f {fGoal = hlgoal}
