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
module Unison.Construction.AddDelimiters (addDelimiters) where

import Unison.Base
import Unison.Util
import Unison.Constructors
import Unison.Predicates
import Unison.Instances()

addDelimiters f @ Function {fCode = code} _target =
  let id      = newId code
      code'   = zipWith (curry addOut) [id..] code
      id'     = newId code'
      code''  = zipWith (curry addIn) [id'..] code'
      code''' = map (removeExit . removeEntry) code''
  in f {fCode = code'''}

addIn (id, b @ Block {bCode = code})  = b {bCode = mkIn id [] : code}
addOut (id, b @ Block {bCode = code}) = b {bCode = code ++ [mkOut id []]}

removeEntry b @
  Block {bAs = as, bCode = (_ : (i @ SingleOperation {oId = id}) : rest)}
  | isEntry i = b {bAs = as {aEntry = True},
                   bCode = mkIn id (entries i) : rest}
removeEntry b = b

removeExit b @ Block {bAs = as, bCode = code} =
  case last (init code) of
    ei @ SingleOperation {} | isExit ei || isReturn ei ->
      (let code'                          = init code
           SingleOperation {oId = id} = last code
       in b {bAs = as {aExit = True, aReturn = isReturn ei},
             bCode = init code' ++ [mkOut id $ returns $ last code']})
    _ -> b

entries (SingleOperation {oOpr = Virtual (Delimiter (Entry e))})  = e

returns (SingleOperation {oOpr = Virtual (Delimiter (Return r))}) = r
returns (SingleOperation {oOpr = Virtual (Delimiter Exit)})       = []