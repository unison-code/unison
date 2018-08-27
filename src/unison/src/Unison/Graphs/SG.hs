{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Same-placement Graph.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Graphs.SG (fromFunction, fromCongruences, complete,
       toNodeId, tempPartitionMap, operandPartitionMap, sameTempPartitions,
       sameOperandPartitions, toTempDot, toOperandDot) where

import Data.Maybe
import Data.List
import qualified Data.Map as M

import Common.Util

import Unison.Base
import Unison.Constructors
import Unison.Instances()
import Unison.Util
import qualified Unison.Graphs.Partition as P
import Unison.Target.API

fromFunction ::
  Eq i => Show i => Ord r => Show r =>
  Maybe (AlignedPairsFunction i r) -> Function i r -> Partition (Operand r)
fromFunction mapf Function {fCode = code, fCongruences = cs} =
  let ops = nub $ concatMap oModelOps (flatten code)
      sg  = fromCongruences cs
      sg1 = P.complete sg (map toCongruenceOp ops)
      sg2 = foldl P.connectElements sg1 (dataCongruences code)
      sg3 = case mapf of
        Just apf -> foldl P.connectElements sg2
                    (mandatoryAlignmentCongruences apf code)
        Nothing -> sg2
  in sg3

fromCongruences :: Show r => [CongruenceTuple r] -> Partition (Operand r)
fromCongruences = P.fromPairList

complete sg = P.complete sg

toNodeId = fromInteger . tId

tempPartitionMap :: Ord r => Partition (Operand r) ->
                    M.Map (Operand r) Integer
tempPartitionMap = partitionMap mkTemp

operandPartitionMap :: Ord r => Partition (Operand r) ->
                       M.Map (Operand r) Integer
operandPartitionMap = partitionMap (mkOperandRef . toInteger)

partitionMap c sg = M.mapKeys c (P.toPartitionMap sg)

sameTempPartitions    = samePartitions mkTemp
sameOperandPartitions = samePartitions mkOpRef

samePartitions c sg = [map c cs | cs <- P.toList sg]

mkOpRef = mkOperandRef . toInteger

toTempDot p = P.toDot mkTemp p
toOperandDot p = P.toDot mkOpRef p

dataCongruences :: Eq r => [Block i r] -> [CongruenceTuple r]
dataCongruences = concatMap blockDataCongruences

blockDataCongruences Block {bCode = code} =
  let ps = concat [[p | p <- oAllOps o, isSingletonChoice p] | o <- code]
  in concatMap (dataCongruences' code) ps

dataCongruences' code p = concatMap (instructionDataCongruence p) code

instructionDataCongruence p = mapMaybe (operandDataCongruence p) . oUseOperands

operandDataCongruence p q
  | p `isEquivalentTo` q && p /= q = Just $ mapTuple toCongruenceOp (p, q)
  | otherwise = Nothing

isSingletonChoice MOperand {altTemps = [_]} = True
isSingletonChoice _ = False

mandatoryAlignmentCongruences apf code =
  concatMap (oprMandatoryAlignmentCongruences apf) (flatten code)

oprMandatoryAlignmentCongruences :: Eq i => Show i => Ord r =>
  AlignedPairsFunction i r -> BlockOperation i r -> [(Operand r, Operand r)]
oprMandatoryAlignmentCongruences apf o =
  let ap2i = fromListMult [((p, q), i) | (p, q, i) <- apf o]
      aps  = [(p, q) | ((p, q), is) <- M.toList ap2i,
              length is == length (oInstructions o)]
  in aps
