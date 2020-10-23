{-|
Copyright   :  Copyright (c) 2019, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org
-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Tools.Import.MovePhiUndefs (movePhiUndefs) where

import Data.List
import Data.Maybe

import Unison

movePhiUndefs f @ Function {fCode = code} _ =
    let code' = fixpoint movePhiUndef code
    in f {fCode = code'}

movePhiUndef code =
  case find (isPhiDefine code) (flatten code) of
   Nothing -> code
   Just d ->
     let code1 = filterCode (not . isIdOf d) code
         p     = fromJust $ definePhi code d
         src   = fromJust $ lookup (oSingleDef d) (phiUses p)
         code2 = applyToBlock (insertWhen before isBranch [d]) code1 src
     in code2

isPhiDefine code = isJust . definePhi code

definePhi code d @ SingleOperation {oOpr = Virtual Define {oDefineDs = [t]}} =
  case users t (flatten code) of
   [p] | isPhi p && oprBlock code d == oprBlock code p -> Just p
   _ -> Nothing
definePhi _ _ = Nothing
