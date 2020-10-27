{-|
Copyright   :  Copyright (c) 2020, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.Minimal.SpecsGen (module X) where
  import Unison.Target.Minimal.SpecsGen.ReadWriteInfo as X
  import Unison.Target.Minimal.SpecsGen.OperandInfo as X
  import Unison.Target.Minimal.SpecsGen.ReadOp as X
  import Unison.Target.Minimal.SpecsGen.ShowInstance()
  import Unison.Target.Minimal.SpecsGen.AllInstructions as X
  import Unison.Target.Minimal.SpecsGen.Itinerary as X
  import Unison.Target.Minimal.SpecsGen.InstructionType as X
  import Unison.Target.Minimal.SpecsGen.AlignedPairs as X
  import Unison.Target.Minimal.SpecsGen.Parent as X
  import Unison.Target.Minimal.SpecsGen.Size as X
