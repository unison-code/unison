module Unison.Target.Minimal.MinimalResourceDecl (MinimalResource (..)) where

data MinimalResource =
  -- Size of each instruction
  BundleWidth |
  -- Simple resource to enforce single-issue
  Issue
  deriving (Eq, Ord, Show, Read)
