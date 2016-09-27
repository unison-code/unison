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
module Unison.Tools.Model.Presolver (presolver) where

import qualified Data.Map as M
import Data.Aeson (toJSON)

import qualified Unison.Tools.Model.PresolverParameters as EP
import Unison.Tools.Model.Definitions

presolver oldModel aux target f ps =
  let sps = toJSON (M.fromList (EP.parameters oldModel aux f target ps))
  in unionMaps ps sps
