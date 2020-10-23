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
module Unison.Tools.Export.RemoveRedundancies (removeRedundancies) where

import qualified Data.Set as S

import Unison

removeRedundancies f @ Function {fCode = code} _target =
    let code' = map removeBlockRedundancies code
    in f {fCode = code'}

removeBlockRedundancies b @ Block {bCode = code} =
    b {bCode = map removeBundleRedundancies code}

removeBundleRedundancies (Bundle code) =
    let (code', _) = foldl removeDuplicates ([], S.empty) code
    in Bundle code'

removeDuplicates (code, instrs) i =
    let inst = oOpr i
    in if S.member inst instrs
       then (code, instrs)
       else (code ++ [i], S.insert inst instrs)
