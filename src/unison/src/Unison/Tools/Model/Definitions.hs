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

instance ToJSON (ConstraintExpr i rc) where
  toJSON e @ (OrExpr es) = toJSON ([toJSON (exprId e)] ++ map toJSON es)
  toJSON e @ (AndExpr es) = toJSON ([toJSON (exprId e)] ++ map toJSON es)
  toJSON e @ (XorExpr e1 e2) = toJSON (exprId e, e1, e2)
  toJSON e @ (ImpliesExpr e1 e2) = toJSON (exprId e, e1, e2)
  toJSON e @ (NotExpr e1) = toJSON (exprId e, e1)
  toJSON e @ (ActiveExpr oid) = toJSON (exprId e, oid)
  toJSON e @ (ConnectsExpr pid tid) = toJSON (exprId e, pid, tid)
  toJSON e @ (EImplementsExpr oid ii) = toJSON (exprId e, oid, ii)
  toJSON e @ (DistanceExpr oid1 oid2 d) = toJSON (exprId e, oid1, oid2, d)
  toJSON e @ (ShareExpr pid1 pid2) = toJSON (exprId e, pid1, pid2)
  toJSON e @ (OperandOverlapExpr pid1 pid2) = toJSON (exprId e, pid1, pid2)
  toJSON e @ (TemporaryOverlapExpr tid1 tid2) = toJSON (exprId e, tid1, tid2)
  toJSON e @ (CallerSavedExpr tid) = toJSON (exprId e, tid)
  toJSON e @ (EAllocatedExpr pid irc) = toJSON (exprId e, pid, irc)
  toJSON e @ (AlignedExpr pid1 pid2 d) = toJSON (exprId e, pid1, pid2, d)

exprId :: ConstraintExpr i rc -> Integer
exprId OrExpr {}               = 0
exprId AndExpr {}              = 1
exprId XorExpr {}              = 2
exprId ImpliesExpr {}          = 3
exprId NotExpr {}              = 4
exprId ActiveExpr {}           = 5
exprId ConnectsExpr {}         = 6
exprId EImplementsExpr {}      = 7
exprId DistanceExpr {}         = 8
exprId ShareExpr {}            = 9
exprId OperandOverlapExpr {}   = 10
exprId TemporaryOverlapExpr {} = 11
exprId CallerSavedExpr {}      = 12
exprId EAllocatedExpr {}       = 13
exprId AlignedExpr {}          = 14

unionMaps (Object m1) (Object m2) = Object (HM.union m1 m2)
