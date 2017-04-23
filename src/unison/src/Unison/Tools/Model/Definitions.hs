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
module Unison.Tools.Model.Definitions
    (mapList, toValueList, toValueListM, blockMap, unionMaps) where

import Data.List
import Data.Ord
import qualified Data.Map as M
import Data.Aeson
import qualified Data.HashMap.Strict as HM

import Common.Util

import Unison

mapList = map . ( . toSingleton)

toValueListM :: Ord k => M.Map k a -> [a]
toValueListM = toValueList . M.toList

toValueList :: Ord k => [(k, a)] -> [a]
toValueList = map snd . sortBy (comparing fst)

blockMap f = map (f . bCode)

-- Functions to map function elements into JSON

instance Show r => ToJSON (Operand r) where
  toJSON (Temporary t _)                    = toJSON t
  toJSON (Register r)                       = toJSON r
  toJSON (MOperand {operandId = id}) = toJSON id
  toJSON NullTemporary                      = toJSON (-1 :: Integer)
  toJSON (OperandRef id)                    = toJSON id

instance Show r => ToJSON (RegisterId r) where
  toJSON r = toJSON (show r)

instance ToJSON (Block i r) where toJSON = toJSON . bLab

instance ToJSON (BlockOperation i r) where toJSON = toJSON . oId

instance ToJSON (IndexedRegisterClass rc) where toJSON = toJSON . rcId

instance ToJSON RegisterSpace where toJSON = toJSON . rsId

instance ToJSON RegisterAtom where toJSON = toJSON . raId

instance ToJSON (IndexedResource s) where toJSON = toJSON . resId

instance ToJSON (IndexedUsage s) where
  toJSON (IndexedUsage resId (Usage _ units occ off)) =
    toJSON (resId, units, occ, off)

instance ToJSON (IndexedInstruction i) where toJSON = toJSON . ioId

instance ToJSON ConstraintExpr where
  toJSON e @ (XorExpr e1 e2) = toJSON (exprId e, e1, e2)
  toJSON e @ (AndExpr e1 e2) = toJSON (exprId e, e1, e2)
  toJSON e @ (ActiveOperation oid) = toJSON (exprId e, oid)

exprId :: ConstraintExpr -> Integer
exprId (XorExpr _ _)       = 0
exprId (AndExpr _ _)       = 1
exprId (ActiveOperation _) = 2

unionMaps (Object m1) (Object m2) = Object (HM.union m1 m2)
