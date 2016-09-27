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
module SpecsGen.ItineraryGen (emitItinerary) where

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

emitItinerary targetName is =
    let us2ids = infoToIds iItinerary is
        rhss   = map (mkOpcRhs idToHsCon toItineraryRhs) us2ids
    in [hsModule
        (moduleName targetName "Itinerary")
        (Just [hsExportVar "itinerary"])
        [instructionDeclImport targetName, itineraryDeclImport targetName]
        [simpleOpcFunBind "itinerary" rhss]]

toItineraryRhs = toHsCon
