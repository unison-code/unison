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
module SpecsGen.ItineraryDeclGen (emitItineraryDecl) where

import Data.List

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitItineraryDecl targetName is =
    let ids = nub $ map iItinerary is
    in [hsModule
        (moduleName targetName (targetName ++ "ItineraryDecl"))
        (Just [hsExportDataType (targetName ++ "Itinerary")])
        []
        [hsItinDecl targetName ids]]
