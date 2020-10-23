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
module SpecsGen.SimpleYaml
    (
     YElement (..),
     YOperandInfo (..),
     YRWObject (..),
     simplify,
     ySeq,
     yMap,
     yString,
     yStringLists,
     yFetch,
     yFetchStr,
     yLookup,
     yInstructions,
     yApply,
     yReplaceString,
     oId,
     iType,
     iLatency,
     iSize,
     iIssueSlots,
     iFunctionalUnits,
     iOperands,
     iUses,
     iDefines,
     iItinerary,
     iUseDefs,
     iAffects,
     iAffectedBy,
     iOperandDelays,
     iParent,
     simplifyOperand,
     toOperandDelayMap,
     toAffectsList
    )
    where

import Data.Yaml
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T

data YElement
    = YMap [(YElement, YElement)]
    | YSeq [YElement]
    | YString String
    | YNil
    deriving (Show, Ord, Eq)

data YOperandInfo
    = YRegisterInfo String String (Maybe Integer)
    | YBlockRefInfo
    | YBoundInfo
    deriving (Show, Ord, Eq)

data YRWObject
    = YMemory String
    | YAllMemory
    | YControlSideEffect
    | YProgramCounterSideEffect
    | YOtherSideEffect String
    deriving (Show, Ord, Eq)

simplify (Object m) = YMap [(simplify (String a), simplify b) | (a, b) <- M.toList m]
simplify (Array a)  = YSeq (map simplify (V.toList a))
simplify (String s) = YString (T.unpack s)
simplify (Number n) = YString (show n)
simplify Null       = YNil
simplify other      = error ("unmatched: simplify " ++ show other)

ySeq (YSeq s)     = s
ySeq (YString "") = []
ySeq YNil         = []
ySeq o            = [o]

yMap (YMap s) = s

yFetchStr key = yString . yFetch key

yFetch key m = case yLookup key m of
  (Just e) -> e
  Nothing  -> YNil

yLookup key (YMap m) = lookup (YString key) m

yString (YString s) = s
yString other = error ("unmatched: yString " ++ show other)

yStringLists (YString s) = [s]
yStringLists (YSeq s) = map yString s

instructionSet = ySeq . yFetch "instruction-set"

yInstructions = concatMap groupInstructions . instructionSet

groupInstructions group = ySeq $ yFetch "instructions" group

yApply f (YMap s) = YMap (map (\(a, b) -> (yApply f a, yApply f b)) s)
yApply f (YSeq s) = YSeq (map (yApply f) s)
yApply f y        = f y

yReplaceString old new str
  | str == old = new
  | otherwise  = str

oId = yFetchStr "id"
iType = yFetchStr "type"
iLatency = const 1
iSize i =
  case yFetch "size" i of
    YNil -> 1
    s    -> yInteger s :: Integer
iIssueSlots = ySeq . yFetch "issue-slots"
iFunctionalUnits = ySeq . yFetch "functional-units"
iOperands = ySeq . yFetch "operands"
iUses = ySeq . yFetch "uses"
iDefines = ySeq . yFetch "defines"
iItinerary = yFetchStr "itinerary"

iUseDefs i = (map yString $ iUses i, map yString $ iDefines i)

iAffects    = iAffectInfo "affects"
iAffectedBy = iAffectInfo "affected-by"

iAffectInfo name = ySeq . yFetch name

iOperandDelays = ySeq . yFetch "operand-delays"
iParent i =
  case yFetch "parent" i of
   YNil -> Nothing
   YString p -> Just p

simplifyOperand (YMap [(n1, n2)]) = (yString n1, toYOperandInfo $ ySeq n2)
simplifyOperand e = error ("unmatched: simplifyOperand " ++ show e)

toYOperandInfo [YString "register", ud, rc, l] =
    YRegisterInfo (yString ud) (yString rc) (Just (yInteger l))

toYOperandInfo [YString "register", ud, rc] =
    YRegisterInfo (yString ud) (yString rc) Nothing

toYOperandInfo [YString "label"] = YBlockRefInfo

toYOperandInfo [YString "bound"] = YBoundInfo

toYOperandInfo other = error ("unmatched: toYOperandInfo " ++ show other)

yInteger s = readInteger (yString s)

toOperandDelayMap = map simplifyOperandDelay

simplifyOperandDelay = toOperandDelay . listElement

toOperandDelay (YString op, YString d) = (op, readInteger d)

toAffectsList [YString s] = [toRWObject (YString s, YString "")]
toAffectsList [] = []
toAffectsList (YMap rws:rest) = map (toRWObject) rws ++ toAffectsList rest
toAffectsList other = error ("unmatched: toAffectsList " ++ show other)

toRWObject (YString m, YString "memory") = YMemory m
toRWObject (YString "mem", _) = YMemory "mem"
toRWObject (YString e, _) = YOtherSideEffect e
toRWObject other = error ("unmatched: toRWObject " ++ show other)

listElement l = let [m] = yMap l in m

readInteger string = round (read string :: Double) :: Integer
