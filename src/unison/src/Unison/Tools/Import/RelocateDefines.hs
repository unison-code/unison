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
{-# LANGUAGE FlexibleContexts, PatternGuards #-}
module Unison.Tools.Import.RelocateDefines (relocateDefines) where

import Data.List

import Common.Util
import Unison

relocateDefines f _target = fixpoint relocateDefine f

relocateDefine f @ Function {fCode = code}
    | Nothing <- findRelocatableDefine code = f
relocateDefine f @ Function {fCode = code} =
    let (Just d) = findRelocatableDefine code
        u        = singleUser d (flatten code)
        code'    = moveGloballyOperation d before (isIdOf u) code
    in f {fCode = code'}

findRelocatableDefine code = find (isRelocatableDefine fCode) fCode
    where fCode = flatten code

isRelocatableDefine code i
    | isDefine i && length (oDefs i) == 1 =
        case users (oSingleDef i) code of
          [u] ->
            let is = between (isIdOf i) (isIdOf u) code
            in not (isPhi u) && any (not . isDefine) is
          _   -> False
    | otherwise = False

singleUser i code = fromSingleton (users (oSingleDef i) code)
