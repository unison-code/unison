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

import Data.List
import qualified Data.Set as S

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Constructors
import qualified Unison.Graphs.SG as SG

-- | Gives a set of callee-saved temporaries
calleeSavedTemps csf ovf f @ Function {fCode = code} =
  let fCode = flatten code
      sg    = SG.fromFunction Nothing f
      sp    = SG.sameTempPartitions sg
      csr   = map (mkRegister . mkTargetRegister) csf
      csp   = filter (isCalleeSavedPartition ovf csr fCode) sp
  in S.fromList $ concat csp

isCalleeSavedPartition ovf csr code =
  all (\t -> isOnlyRefByBoundaries code t &&
             isCalleeSavedOverlap ovf csr code t)

isOnlyRefByBoundaries code t =
    let usrs  = filter (isUser t) code
        dfrs  = filter (isDefiner t) code
    in all isIn dfrs && all isOut usrs

isCalleeSavedOverlap ovf csr code t =
  let rs = nub [r | (_, t', r) <- preAssignments [mkDummyBlock code], t == t']
  in all (\r -> any (\r' -> ovf r r') csr) rs
