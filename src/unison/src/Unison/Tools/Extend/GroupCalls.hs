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
module Unison.Tools.Extend.GroupCalls (groupCalls) where

import Data.List.Split

import Unison

groupCalls f @ Function {fCode = code} _target =
    let code' = map groupCallsInBlock code
    in f {fCode = code'}

groupCallsInBlock b @ Block {bCode = code} =
    let code'  = groupOperations (True, isCall, isFun) code
        code'' = groupOperations (False, isFun, isKill) code'
    in b {bCode = code''}

groupOperations (left, f1, f2) code =
    let codes  = split (whenElt (\o -> f1 o || f2 o)) code
        codes' = groupOperations' (left, f1, f2) codes
    in concat codes'

groupOperations' (left, f1, f2) ([o1]:(os:([o2]:codes))) | f1 o1 && f2 o2 =
    (if left then os else []):[o1]:[o2]:(if left then [] else os):
    groupOperations' (left, f1, f2) codes
groupOperations' info (os:codes) = os:groupOperations' info codes
groupOperations' _ [] = []

