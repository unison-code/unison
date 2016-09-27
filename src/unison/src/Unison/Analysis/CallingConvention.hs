{-|
Copyright   :  Copyright (c) 2016, SICS Swedish ICT AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@sics.se

Algorithms to compute properties related to calling conventions.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Analysis.CallingConvention (calleeSavedTemps) where

import qualified Data.Set as S

import Unison.Base
import Unison.Util
import Unison.Predicates
import qualified Unison.Graphs.SG as SG

-- | Gives a set of callee-saved temporaries
calleeSavedTemps f @ Function {fCode = code} =
  let fCode  = flatten code
      sg     = SG.fromFunction Nothing f
      sp     = SG.sameTempPartitions sg
      csp    = filter (isCalleeSavedPartition fCode) sp
  in S.fromList $ concat csp

isCalleeSavedPartition code = all (isOnlyRefByBoundaries code)

isOnlyRefByBoundaries code t =
    let usrs  = filter (isUser t) code
        dfrs  = filter (isDefiner t) code
    in all isIn dfrs && all isOut usrs
