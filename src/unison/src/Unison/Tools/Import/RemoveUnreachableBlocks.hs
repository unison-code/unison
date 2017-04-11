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
module Unison.Tools.Import.RemoveUnreachableBlocks
       (removeUnreachableBlocks) where

import Data.Graph.Inductive
import qualified Data.Set as S

import Unison
import Unison.Target.API
import qualified Unison.Graphs.BCFG as BCFG

removeUnreachableBlocks f @ Function {fCode = code} target =
    let bif   = branchInfo target
        bcfg  = BCFG.fromFunction bif f
        rbids = S.fromList $ reachable (bNode (entryBlock code)) bcfg
        code' = filter (\b -> S.member (bNode b) rbids) code
    in f {fCode = code'}

bNode = BCFG.toNode . bLab
