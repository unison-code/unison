module Unison.Target.ARM.ARMResourceDecl (ARMResource (..)) where

data ARMResource =
  BundleWidth |
  V6_Pipe
  deriving (Eq, Ord, Show, Read)
