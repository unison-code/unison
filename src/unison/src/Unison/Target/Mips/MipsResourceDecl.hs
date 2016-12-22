module Unison.Target.Mips.MipsResourceDecl (MipsResource (..)) where

data MipsResource =
  -- Size of each instruction
  BundleWidth |
  -- Resource to model delay-slot instructions
  Issue |
  -- Resource to avoid overlaps between long-latency and delay-slot
  -- instructions: the former consume the resource unders all execution (but not
  -- issue) cycles, the latter consumes the resource at its issue cycle
  LongDuration |
  -- LLVM itinerary resources
  ALU |
  IMULDIV
  deriving (Eq, Ord, Show, Read)
