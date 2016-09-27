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
{-# LANGUAGE FlexibleContexts #-}
module Unison.Tools.Analyze.GoalParser (parseGoals) where

import Unison
import Text.ParserCombinators.Parsec

data PGoalDescription = PGoalDescription String PGoal
data PGoal = PStaticGoal PGoalObject | PDynamicGoal PGoalObject
data PGoalObject = PCycles | PResourceUsage String

parseGoals goals = case parse goalList "" goals of
                     Left e   -> error ("error parsing goals:\n" ++ show e)
                     Right gl -> map toGoalDescription gl

goalList =
  do gl <- goalDescription `sepBy` char ';'
     eof
     return gl

goalDescription =
  do whitespaces
     d <- many1 alphaNum
     whitespaces
     char ':'
     whitespaces
     g <- goal
     whitespaces
     return (PGoalDescription d g)

goal = try staticGoal <|> dynamicGoal

staticGoal  = typedGoal PStaticGoal 's'
dynamicGoal = typedGoal PDynamicGoal 'd'

typedGoal t c =
  do char '('
     char c
     char ')'
     whitespaces
     o <- goalObject
     return (t o)

goalObject = try cycles <|> resourceUsage

cycles =
  do string "cycles"
     notFollowedBy alphaNum
     return PCycles

resourceUsage =
  do r <- many1 alphaNum
     return (PResourceUsage r)

whitespaces = many (oneOf "\t ")

toGoalDescription (PGoalDescription d g) = GoalDescription d (toGoal g)

toGoal (PStaticGoal go) = StaticGoal (toGoalObject go)
toGoal (PDynamicGoal go) = DynamicGoal (toGoalObject go)

toGoalObject PCycles = Cycles
toGoalObject (PResourceUsage r) = ResourceUsage (read r)
