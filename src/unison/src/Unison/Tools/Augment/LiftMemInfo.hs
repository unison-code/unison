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
module Unison.Tools.Augment.LiftMemInfo (liftMemInfo) where

import Data.List

import Unison

liftMemInfo f @ Function {fCode = code} _target =
    let code'  = mapToOperationInBlocks liftMemInfoInOpr code
        code'' = map expandAllMemObjects code'
    in f {fCode = code''}

liftMemInfoInOpr o =
  case aMem $ oAs o of
    Just m ->
      let o' = (mapToReads (liftMemList m) . mapToWrites (liftMemList m)) o
      in o' {oAs = (oAs o') {aMem = Nothing}}
    Nothing  -> o

liftMemList = map . liftMem

liftMem m o
  | isMem o = Memory (show m)
  | otherwise = o

expandAllMemObjects b @ Block {bCode = code} =
  let mems  = filter isMem $ conNub [readObjects o ++ writeObjects o | o <- code]
      code' = map (expandAllMemObjectInOpr mems) code
  in b {bCode = code'}

expandAllMemObjectInOpr mems o =
  let o' = (mapToReads  (expandAllMemObjectList mems) .
            mapToWrites (expandAllMemObjectList mems)) o
  in o'

expandAllMemObjectList = concatMap . expandAllMemObject

expandAllMemObject mems o
  | isAllMem o = mems
  | otherwise = [o]

conNub :: Ord r => [[RWObject r]] -> [RWObject r]
conNub = sort . nub . concat

readObjects  = aReads . oAs
writeObjects = aWrites . oAs

isMem (Memory _) = True
isMem _ = False

isAllMem AllMemory = True
isAllMem _ = False
