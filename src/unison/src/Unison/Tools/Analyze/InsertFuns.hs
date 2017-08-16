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
module Unison.Tools.Analyze.InsertFuns (insertFuns) where

import Unison

insertFuns f @ Function {fCode = code} _target =
    let code' = map insertFunsInBlock code
    in f {fCode = code'}

insertFunsInBlock b @ Block {bCode = code} =
  let code' = insertFun code
  in b {bCode = code'}

insertFun (o:os)
  | any (\o -> isCall o || isTailCall o) (linearizeOpr o) = o:fun:insertFun os
  | otherwise = o:insertFun os
insertFun [] = []

fun = mkFun (-1) [] []
