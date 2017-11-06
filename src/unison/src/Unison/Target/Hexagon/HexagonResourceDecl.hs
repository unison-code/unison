module Unison.Target.Hexagon.HexagonResourceDecl (HexagonResource (..)) where

-- | Declares target architecture resources

data HexagonResource =
  BundleWidth |
  Slot0123 |
  Slot01 |
  Slot0 |
  Slot23 |
  Slot2 |
  Slot3 |
  Store |
  ConNewValue |
  BlockEnd |
  SpillCost
  deriving (Eq, Ord, Show, Read)
