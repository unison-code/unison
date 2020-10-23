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
module Unison.Tools.Import.KillUnusedTemps (killUnusedTemps) where

import Data.List
import Data.Maybe

import Unison

killUnusedTemps f @ Function {fCode = code} _target =
    let uses       = nub $ tUses (flatten code)
        id         = newId code
        (code', _) = foldl (killUnusedTempsInBB uses) ([], id) code
    in f {fCode = code'}

killUnusedTempsInBB uses (accCode, id) b @ Block {bCode = code} =
    let (code', id')  = foldl (killUnusedDefs uses) ([], id) code
    in (accCode ++ [b {bCode = code'}], id')

killUnusedDefs uses (accCode, id) i
  | isCopy i  = (accCode ++ [i], id)
  | otherwise =
    let widows       = filter isTemporary (oDefs i) \\ uses
        widows'      = map undoPreAssign widows
        (kills, id') = mkKills widows' id
    in (accCode ++ [i] ++ maybeToList kills, id')

mkKills [] id = (Nothing, id)
mkKills ws id = (Just (mkKill id [VirtualInstruction] ws), id + 1)
