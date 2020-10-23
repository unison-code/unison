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
module Unison.Target.Hexagon.SpecsGen (module X) where
  import Unison.Target.Hexagon.SpecsGen.ReadWriteInfo as X
  import Unison.Target.Hexagon.SpecsGen.OperandInfo as X
  import Unison.Target.Hexagon.SpecsGen.ReadOp as X
  import Unison.Target.Hexagon.SpecsGen.ShowInstance()
  import Unison.Target.Hexagon.SpecsGen.Itinerary as X
  import Unison.Target.Hexagon.SpecsGen.InstructionType as X
  import Unison.Target.Hexagon.SpecsGen.AlignedPairs as X
  import Unison.Target.Hexagon.SpecsGen.Parent as X
  import Unison.Target.Hexagon.SpecsGen.AllInstructions as X
