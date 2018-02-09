{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.ARM.SpecsGen (module X) where
  import Unison.Target.ARM.SpecsGen.ReadWriteInfo as X
  import Unison.Target.ARM.SpecsGen.OperandInfo as X
  import Unison.Target.ARM.SpecsGen.ReadOp as X
  import Unison.Target.ARM.SpecsGen.ShowInstance()
  import Unison.Target.ARM.SpecsGen.Itinerary as X
  import Unison.Target.ARM.SpecsGen.InstructionType as X
  import Unison.Target.ARM.SpecsGen.AlignedPairs as X
  import Unison.Target.ARM.SpecsGen.Size as X
  import Unison.Target.ARM.SpecsGen.Parent as X
