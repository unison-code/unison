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
module Unison.Tools.Augment.ExpandCopies (expandCopies) where

import Data.List
import Data.Maybe

import Unison
import Unison.Target.API

expandCopies f @ Function {fCode = code} target =
    let ecf        = expandCopy target
        fcode      = flatten code
        ids        = (newOprIndex fcode, newOperIndex fcode, newTempIndex fcode)
        (_, code') = mapAccumL (expandCopiesInBlock ecf) ids code
    in f {fCode = code'}

expandCopiesInBlock ecf ids b @ Block {bCode = code} =
  let cs    = filter isCopy code
      (code', ids') = foldl (doExpandCopy (ecf b)) (code, ids) (map oId cs)
  in (ids', b {bCode = code'})

doExpandCopy ecf (code, ids) cid =
  let c      = fromJust $ find (isId cid) code
      -- 1. expand the copy according to expandCopy (target function)
      os     = ecf ids c
      code'  = replace c os code
      -- 2. replace copy destination with alt temps defined by the expanded copies
      t      = copyDefinition c
      ts     = [copyDefinition o | o <- os, isCopy o]
      code'' = map (mapToModelOperand (replaceTemp t ts)) code'
      -- 3. update indexes
      ids'   = updateIndexes ids code''
  in (code'', ids')

copyDefinition = fromSingleton . extractTemps . copyDestination

replaceTemp t ts p @ MOperand {altTemps = ats} =
  let ats' = concatMap (\t' -> if t' == t then ts else [t']) ats
  in p {altTemps = ats'}

replace o os code =
  let code'  = insertWhen after (== o) os code
      code'' = delete o code'
  in code''

updateIndexes (oid, pid, tid) code =
  let oid' = max oid (newOprIndex code)
      pid' = max pid (newOperIndex code)
      tid' = max tid (newTempIndex code)
  in (oid', pid', tid')
