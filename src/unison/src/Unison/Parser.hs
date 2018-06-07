{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Support to parse Unison functions.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE FlexibleContexts #-}
module Unison.Parser (Unison.Parser.parse) where

import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split hiding (endBy, sepBy, oneOf)
import Text.ParserCombinators.Parsec hiding (sourceLine)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Arrow

import Common.Util hiding (between)

import MachineIR.Base (MachineOperand, MachineIRVersion (..),
                       mfiIndex, mjtiIndex, mcpiIndex)
import MachineIR.Parser

import Unison.Base
import Unison.Constructors
import Unison.Util
import Unison.Instances()
import Unison.Target.API

data LsLine r =
  LsComment String |
  LsOperation OperationId [LsInstruction] ([LsOperand r], [LsOperand r])
  (LsAttributes r) |
  LsBlock BlockId LsBlockAttributes
  deriving (Show)

data LsInstruction =
  LsNullInstruction |
  LsVirtualInstruction String |
  LsTargetInstruction String
  deriving (Show)

data LsOperand r =
  LsTemporary TemporaryId (Maybe (LsOperand r)) |
  LsNullTemporary |
  LsMOperand MoperandId [LsOperand r] (Maybe (LsOperand r)) |
  LsBlockRef BlockId |
  LsRegister String |
  LsBound (MachineOperand r) |
  LsOperandRef MoperandId
  deriving (Show)

data LsAttributes r =
  LsAttributes (LsAttribute r) (LsAttribute r) (LsAttribute r) (LsAttribute r)
               (LsAttribute r) (LsAttribute r) (LsAttribute r) (LsAttribute r)
               (LsAttribute r) (LsAttribute r) (LsAttribute r) (LsAttribute r)
  deriving (Show)

data LsAttribute r =
  LsReads [LsRWObject] |
  LsWrites [LsRWObject] |
  LsAttrCall (Maybe OperationId) |
  LsMem (Maybe Integer) |
  LsActivators [LsInstruction] |
  LsVirtualCopy Bool |
  LsRemat Bool |
  LsJTBlocks [LsOperand r] |
  LsBranchTaken (Maybe Bool) |
  LsPrescheduled (Maybe IssueCycle) |
  LsRematOrigin (Maybe OperationId) |
  LsSplitBarrier Bool
  deriving (Show)

data LsRWObject =
  LsMemory String |
  LsAllMemory |
  LsControlSideEffect |
  LsProgramCounterSideEffect |
  LsOtherSideEffect String
  deriving (Show)

data LsBlockAttributes =
  LsBlockAttributes [LsBlockAttribute] deriving (Show)

data LsBlockAttribute =
  LsEntry |
  LsExit |
  LsReturn |
  LsFreq Frequency |
  LsSplit
  deriving (Eq, Show)

data LsHighLevelGoal =
  LsSpeed |
  LsSize |
  LsSpill
  deriving (Eq, Show)

data LsFrameObject =
  LsFrameObject Integer Integer (Maybe Integer) Integer (Maybe String)
  deriving (Eq, Show)

data LsConstantObject = LsConstantObject Integer String Integer
  deriving (Eq, Show)

parse :: Read i => Read r =>
         TargetWithOptions i r rc s -> String -> Function i r
parse target s =
  case Text.ParserCombinators.Parsec.parse sFile "" s of
    Left e -> error ("error parsing input:\n" ++ show e)
    Right r -> toFunction target r

sFile =
  do comms  <- commentLine `endBy` newline
     name   <- functionName
     body   <- bodyLine `endBy` newline
     congruencesMarkerLine
     cs     <- congruenceLine `endBy` newline
     rematerializableMarkerLine
     rts    <- rematerializableLine `endBy` newline
     fixedframeMarkerLine
     ffobjs <- frameObjectLine `endBy` newline
     frameMarkerLine
     fobjs  <- frameObjectLine `endBy` newline
     sp     <- spOffsetLine
     ss     <- stackArgSizeLine
     constantsMarkerLine
     consts <- constantObjectLine `endBy` newline
     jumpTableMarkerLine
     jtk    <- option [] (jumpTableKindLine `endBy` newline)
     jtes   <- jumpTableEntryLine `endBy` newline
     goal   <- goalLine
     rfs    <- removedFreqsLine
     sourceMarkerLine
     src    <- sourceLine `endBy` newline
     eof
     return (comms, name, body, listToMaybe cs, listToMaybe rts, ffobjs, fobjs,
             sp, ss, consts, (toKind jtk, jtes), goal, rfs, src)

toKind [] = ""
toKind [k] = k

functionName =
  do marker "function"
     whiteSpace
     name <- many alphaNumDashAtDotUnderscore
     whiteSpace
     return name

commentLine          = tryLineOf comment
bodyLine             = tryLineOf (bLabelDeclaration <|> blockOperation)
congruenceLine       = tryLineOf congruenceList
rematerializableLine = tryLineOf rematTupleList
frameObjectLine      = tryLineOf frameObject
constantObjectLine   = tryLineOf constantObject
jumpTableKindLine    = tryLineOf jumpTableKind
jumpTableEntryLine   = tryLineOf jumpTableEntry
sourceLine           = tryLineOf source

tryLineOf = try . lineOf

congruencesMarkerLine = lineOf (marker "adjacent")
rematerializableMarkerLine = lineOf (marker "rematerializable")
fixedframeMarkerLine  = lineOf (marker "fixed-frame")
frameMarkerLine       = lineOf (marker "frame")
constantsMarkerLine   = lineOf (marker "constants")
jumpTableMarkerLine   = lineOf (marker "jump-table")
sourceMarkerLine      = lineOf (marker "source")

spOffsetLine = lineOf spOffset
stackArgSizeLine = lineOf stackArgSize
goalLine = lineOf goal
removedFreqsLine = lineOf removedFreqs

goal =
  do marker "goal"
     whiteSpace
     goals <- goalName `sepBy` comma
     whiteSpace
     return goals

removedFreqs =
  do marker "removed-freqs"
     whiteSpace
     rfs <- decimal `sepBy` comma
     whiteSpace
     return rfs

spOffset =
  do marker "stack-pointer-offset"
     whiteSpace
     sp <- decimal
     whiteSpace
     return sp

stackArgSize =
  do marker "stack-arg-size"
     whiteSpace
     sp <- decimal
     whiteSpace
     return sp

goalName = try speedGoal <|> try sizeGoal <|> try spillGoal

speedGoal =
  do string "speed"
     return LsSpeed

sizeGoal =
  do string "size"
     return LsSize

spillGoal =
  do string "spill"
     return LsSpill

lineOf e =
  do whiteSpace
     e

comment =
  do string "//"
     c <- many (noneOf "\n")
     return (LsComment c)

bLabelDeclaration =
  do l <- bLabel
     whiteSpace
     as <- option mkNullLsBlockAttributes blockAttributes
     whiteSpace
     char ':'
     return (LsBlock l as)

blockAttributes =
  do string "("
     attrs <- blockAttribute `sepBy` comma
     string ")"
     return (LsBlockAttributes attrs)

blockAttribute = boolBlockAttribute <|> integerAttribute "freq" LsFreq

boolBlockAttribute = try (simpleAttribute "exit" LsExit) <|>
                     simpleAttribute "entry" LsEntry <|>
                     simpleAttribute "return" LsReturn <|>
                     simpleAttribute "split" LsSplit

simpleAttribute n t =
  do string n
     whiteSpace
     return t

blockOperation =
  do id <- operationId
     whiteSpace
     char ':'
     whiteSpace
     ds <- operandList
     whiteSpace
     string "<-"
     whiteSpace
     is <- instructions
     whiteSpace
     us <- operandList
     as <- option mkNullLsAttributes attributes
     return (LsOperation id is (us, ds) as)

operationId =
  do char 'o'
     id <- decimal
     return id

instructions = try manyInstructions <|> singleInstruction

manyInstructions = braces (instruction `sepBy` comma)

singleInstruction =
    do inst <- instruction
       return [inst]

instruction = nullInstruction <|> virtualInstruction <|> targetInstruction

nullInstruction =
    do char '-'
       return LsNullInstruction

virtualInstruction =
    do inst <- parens nonNullInstruction
       return (LsVirtualInstruction inst)

targetInstruction =
    do inst <- nonNullInstruction
       return (LsTargetInstruction inst)

nonNullInstruction = many1 alphaNumDashAtDotUnderscore

operandList = brackets (operand `sepBy` comma)

operand = temporary <|> mOperand <|> labelRef <|> bound

attributes =
    do string " ("
       attrs <- attribute `sepBy` comma
       string ")"
       return (LsAttributes (fetchAttr (LsReads []) isLsReads attrs)
                            (fetchAttr (LsWrites []) isLsWrites attrs)
                            (fetchAttr (LsAttrCall Nothing) isLsAttrCall attrs)
                            (fetchAttr (LsMem Nothing) isLsMem attrs)
                            (fetchAttr (LsActivators []) isLsActivators attrs)
                            (fetchAttr (LsVirtualCopy False) isLsVirtualCopy attrs)
                            (fetchAttr (LsRemat False) isLsRemat attrs)
                            (fetchAttr (LsJTBlocks []) isLsJTBlocks attrs)
                            (fetchAttr (LsBranchTaken Nothing) isLsBranchTaken attrs)
                            (fetchAttr (LsPrescheduled Nothing) isLsPrescheduled attrs)
                            (fetchAttr (LsRematOrigin Nothing) isLsRematOrigin attrs)
                            (fetchAttr (LsSplitBarrier False) isLsSplitBarrier attrs))

attribute = try (sideEffectListAttribute "reads" LsReads)
            <|> sideEffectListAttribute "writes" LsWrites
            <|> try (operationIdAttribute "call" (LsAttrCall . Just))
            <|> integerAttribute "mem" (LsMem . Just)
            <|> instructionListAttribute "activators" LsActivators
            <|> simpleAttribute "virtualcopy" (LsVirtualCopy True)
            <|> try (operationIdAttribute "remat-origin" (LsRematOrigin . Just))
            <|> simpleAttribute "remat" (LsRemat True)
            <|> operandListAttribute "jtblocks" LsJTBlocks
            <|> boolAttribute "taken" (LsBranchTaken . Just)
            <|> integerAttribute "cycle" (LsPrescheduled . Just)
            <|> simpleAttribute "split-barrier" (LsSplitBarrier True)

sideEffectListAttribute = attributeList sideEffect

integerAttribute = attributeItem decimal

boolAttribute = attributeItem bool

bool = trueValue <|> falseValue

trueValue =
  do string "true"
     return True

falseValue =
  do string "false"
     return False

operationIdAttribute = attributeItem operationId

instructionListAttribute = attributeList instruction

operandListAttribute = attributeList operand

sideEffect = try memorySideEffect
             <|> try allMemorySideEffect
             <|> try controlSideEffect
             <|> try counterSideEffect
             <|> otherSideEffect

memorySideEffect =
  do string "mem-"
     se <- many alphaNum
     return (LsMemory se)

allMemorySideEffect =
  do string "mem"
     notFollowedBy alphaNumDashAtDotUnderscore
     return LsAllMemory

controlSideEffect =
  do string "control"
     notFollowedBy alphaNumDashAtDotUnderscore
     return LsControlSideEffect

counterSideEffect =
  do string "pc"
     notFollowedBy alphaNumDashAtDotUnderscore
     return LsProgramCounterSideEffect

otherSideEffect =
  do se <- many1 alphaNumDashAtDotUnderscore
     return (LsOtherSideEffect se)

attributeList p = attributeItem (brackets (p `sepBy` comma))

attributeItem p n t =
  do marker n
     whiteSpace
     vals <- p
     whiteSpace
     return (t vals)

congruenceList =
  do c <- congruence
     optional comma
     cs <- congruence `sepBy` comma
     return (c:cs)

congruence = temporaryCongruence <|> operandCongruence

temporaryCongruence = congruenceOf temporaryRef
operandCongruence   = congruenceOf operandRef

congruenceOf t =
  do o <- t
     whiteSpace
     string "->"
     whiteSpace
     o' <- t
     return (o, o')

rematTupleList =
  do t <- rematTuple
     optional comma
     ts <- rematTuple `sepBy` comma
     return (t:ts)

rematTuple =
  do t <- temporary
     whiteSpace
     oids <- brackets (operationId `sepBy` comma)
     return (t, oids)

frameObject =
  do mfi <- mirFI
     whiteSpace
     char ':'
     whiteSpace
     off <- signedDecimalObjectProperty "offset"
     comma
     size <- optionMaybe frameSizeAndComma
     align <- signedDecimalObjectProperty "align"
     csreg <- optionMaybe frameCSReg
     return (LsFrameObject (mfiIndex mfi) off size align csreg)

frameSizeAndComma =
  do size <- signedDecimalObjectProperty "size"
     comma
     return size

signedDecimalObjectProperty p =
  do string p
     whiteSpace
     char '='
     whiteSpace
     v <- signedDecimal
     return v

frameCSReg =
  do string "callee-saved-register"
     whiteSpace
     char '='
     whiteSpace
     oneOf "%$"
     r <- many1 alphaNumDashAtDotUnderscore
     return r

constantObject =
  do mci <- mirConstantPoolIndex
     whiteSpace
     char ':'
     whiteSpace
     val <- constantVal
     comma
     align <- signedDecimalObjectProperty "align"
     return (LsConstantObject (read (mcpiIndex mci)) val align)

constantVal =
  do string "value"
     whiteSpace
     char '='
     whiteSpace
     c <- try unstructuredConstantVal <|> try structuredConstantVal
     return c

unstructuredConstantVal =
  do char '\''
     v <- many $ noneOf "\'"
     char '\''
     return v

structuredConstantVal = many1 (noneOf ",")

jumpTableKind =
  do string "kind:"
     whiteSpace
     k <- many1 alphaNumDashAtDotUnderscore
     return k

jumpTableEntry =
  do mjti <- mirJTI
     whiteSpace
     char ':'
     whiteSpace
     bs <- brackets (labelRef `sepBy` comma)
     return (mkJumpTableEntry (mjtiIndex mjti) (map lsBlockRefId bs))

source =
  do s <- many (noneOf "\n")
     return s

temporary =
  do char 't'
     t <- decimal
     r <- option Nothing preAssignedRegister
     return (LsTemporary t r)

preAssignedRegister =
    do char ':'
       r <- register LsRegister
       return (Just r)

mOperand =
    do (LsOperandRef p) <- operandRef
       ts <- braces (connectableTemp `sepBy` comma)
       r  <- option Nothing preAssignedRegister
       return (LsMOperand p ts r)

connectableTemp = temporaryRef <|> nullTemporary

nullTemporary =
    do char '-'
       return LsNullTemporary

temporaryRef =
  do char 't'
     index <- decimal
     return (LsTemporary index Nothing)

register t =
  do r <- many (noneOf ",[]():")
     return (t r)

labelRef =
  do l <- bLabel
     return (LsBlockRef l)

bLabel =
  do string "b"
     decimal

bound =
  do e <- mirOperand LLVM5
     return (LsBound e)

operandRef =
  do char 'p'
     p <- decimal
     return (LsOperandRef p)

marker str =
  do string str
     char ':'
     return ()

lexer = P.makeTokenParser emptyDef
decimal = P.decimal lexer
whiteSpace = P.whiteSpace lexer
comma = P.comma lexer

brackets = between (char '[') (char ']')
parens = between (char '(') (char ')')
braces = between (char '{') (char '}')

signedDecimal =
  do s <- option 1 sign
     dec <- decimal
     return (s * dec)

sign =
  do char '-'
     return (-1)

isAlphaNumDashUnderscore '-' = True
isAlphaNumDashUnderscore '_' = True
isAlphaNumDashUnderscore c = isAlphaNum c

alphaNumDashAtDotUnderscore =
  satisfy isAlphaNumDashAtDotUnderscore <?> "letter, digit, dash, at, dot or underscore symbol"

isAlphaNumDashAtDotUnderscore '@' = True
isAlphaNumDashAtDotUnderscore '.' = True
isAlphaNumDashAtDotUnderscore c   = isAlphaNumDashUnderscore c

fetchAttr :: a -> (a -> Bool) -> [a] -> a
fetchAttr emptyAttr p = fromMaybe emptyAttr . find p

isLsReads (LsReads _) = True
isLsReads _           = False

isLsWrites (LsWrites _) = True
isLsWrites _            = False

isLsAttrCall (LsAttrCall _) = True
isLsAttrCall _              = False

isLsMem (LsMem _) = True
isLsMem _         = False

isLsActivators (LsActivators _) = True
isLsActivators _                 = False

isLsVirtualCopy (LsVirtualCopy _) = True
isLsVirtualCopy _                 = False

isLsRemat (LsRemat _) = True
isLsRemat _           = False

isLsJTBlocks (LsJTBlocks _) = True
isLsJTBlocks _              = False

isLsBranchTaken (LsBranchTaken _) = True
isLsBranchTaken _                 = False

isLsPrescheduled (LsPrescheduled _) = True
isLsPrescheduled _                  = False

isLsRematOrigin (LsRematOrigin _) = True
isLsRematOrigin _                 = False

isLsSplitBarrier (LsSplitBarrier _) = True
isLsSplitBarrier _                 = False

toFunction target
  (cmms, name, body, cs, rts, ffobjs, fobjs, sp, ss, consts, (jtk, jt), goal,
   rfs, src) =
  let cms     = [cm | (LsComment cm) <- cmms]
      code    = map (toBB target) (split (dWhen isLsBB) body)
      cs'     = map (mapTuple toOperand) $ fromMaybe [] cs
      rts'    = map (first toOperand) $ fromMaybe [] rts
      ffobjs' = map toFrameObj ffobjs
      fobjs'  = map toFrameObj fobjs
      consts' = map toConstantObj consts
      goal'   = map toHLGoal goal
      src'    = concat [l ++ "\n" | l <- src]
  in mkCompleteFunction cms name code cs' rts' ffobjs' fobjs' sp ss consts'
     (jtk, jt) goal' rfs src'

toBB target (LsBlock l as : code) =
  Block l (toBlockAttributes as) (map (toOperation target) code)

toBlockAttributes (LsBlockAttributes attrs) =
  mkBlockAttributes (LsEntry `elem` attrs) (LsExit `elem` attrs)
                    (LsReturn `elem` attrs) (fmap lsFreq $ find isLsFreq attrs)
                    (LsSplit `elem` attrs)

isLsFreq (LsFreq _) = True
isLsFreq _ = False

lsFreq (LsFreq f) = f

isLsNull LsNullInstruction = True
isLsNull _ = False

toOperation target (LsOperation id is (us, ds) as) =
  let itf  = instructionType target
      is'  = map readInstr is
      is'' = map oGeneralInstr is'
      us'  = map toOperand us
      ds'  = map toOperand ds
      o = case operationType itf (filter (not . isLsNull) is) of
        (NaturalType nType) ->
          mkNaturalOperation id nType is' us' ds'
        (VirtualType vType) -> mkVirtualOperation id is'' vType (us', ds')
        CopyType -> mkCopy id is' (head us') (tail us') (head ds') (tail ds')
  in o {oAs = toAttributes as}

toOperand :: Read r => LsOperand r -> Operand r
toOperand (LsTemporary tid pas) = mkCompleteTemp tid (fmap toOperand pas)
toOperand LsNullTemporary = mkNullTemp
toOperand (LsRegister name) = mkRegister (read name)
toOperand (LsBound mop) = mkBound mop
toOperand (LsMOperand pid ts pas) =
    mkMOperand pid (map toOperand ts) (fmap toOperand pas)
toOperand (LsBlockRef bid) = mkBlockRef bid
toOperand (LsOperandRef pid) = mkOperandRef pid

toFrameObj (LsFrameObject idx off size ali csreg) =
  mkFrameObject idx off size ali (fmap read csreg)

toConstantObj (LsConstantObject id v a) = (id, v, a)

toHLGoal LsSpeed = Speed
toHLGoal LsSize = Size
toHLGoal LsSpill = Spill

operationType _ [LsVirtualInstruction i] = VirtualType (toVirtualType i)
operationType itf is =
  let is' = map readInstr is
  in case itf (targetInst is') of
    LinearInstructionType -> NaturalType LinearType
    BranchInstructionType -> NaturalType BranchType
    CallInstructionType -> NaturalType CallType
    TailCallInstructionType -> NaturalType TailCallType
    CopyInstructionType -> CopyType

mkNaturalOperation id nType ops us ds =
  case nType of
    LinearType -> mkLinear id ops us ds
    BranchType -> mkBranch id ops us
    CallType -> mkCall id ops us
    TailCallType -> mkTailCall id ops us

mkVirtualOperation id _  PhiType (us, [d]) = mkPhi id us d
mkVirtualOperation id _  (DelimiterType InType)  ([], ds) = mkIn id ds
mkVirtualOperation id _  (DelimiterType OutType) (us, []) = mkOut id us
mkVirtualOperation id is KillType (us, []) = mkKill id is us
mkVirtualOperation id _  DefineType ([], ds) = mkDefine id ds
mkVirtualOperation id _  CombineType ([lu, hi], [d]) = mkCombine id lu hi d
mkVirtualOperation id is LowType ([u], [d]) = mkLow id is u d
mkVirtualOperation id is HighType ([u], [d]) = mkHigh id is u d
mkVirtualOperation id _  Split2Type ([u], [ld, hd]) = mkSplit2 id u ld hd
mkVirtualOperation id _  Split4Type ([u], [lld, lhd, hld, hhd]) =
  mkSplit4 id u lld lhd hld hhd
mkVirtualOperation id _  VirtualCopyType ([s], [d]) = mkVirtualCopy id s d
mkVirtualOperation id _  FunType (us, ds) = mkFun id us ds
mkVirtualOperation id _  (FrameType SetupType) ([s], []) = mkFrameSetup id s
mkVirtualOperation id _  (FrameType DestroyType) ([s], []) = mkFrameDestroy id s

readInstr LsNullInstruction = mkNullInstruction
readInstr (LsVirtualInstruction _) = mkVirtualInstruction
readInstr (LsTargetInstruction i) = TargetInstruction (read i)

toAttributes (LsAttributes (LsReads reads) (LsWrites writes) (LsAttrCall call)
              (LsMem mem) (LsActivators acs) (LsVirtualCopy vcopy)
              (LsRemat rm) (LsJTBlocks bs) (LsBranchTaken bt)
              (LsPrescheduled pa) (LsRematOrigin ro) (LsSplitBarrier sb)) =
  mkAttributes (map toRWObject reads) (map toRWObject writes) call mem
               (map readInstr acs) vcopy rm (map lsBlockRefId bs) bt pa ro sb

lsBlockRefId (LsBlockRef bid) = bid

toRWObject (LsMemory o) = Memory o
toRWObject LsAllMemory = AllMemory
toRWObject LsControlSideEffect = ControlSideEffect
toRWObject LsProgramCounterSideEffect = ProgramCounterSideEffect
toRWObject (LsOtherSideEffect r) = OtherSideEffect (read r)

mkNullLsAttributes =
  LsAttributes (LsReads []) (LsWrites []) (LsAttrCall Nothing) (LsMem Nothing)
  (LsActivators []) (LsVirtualCopy False) (LsRemat False) (LsJTBlocks [])
  (LsBranchTaken Nothing) (LsPrescheduled Nothing) (LsRematOrigin Nothing)
  (LsSplitBarrier False)

mkNullLsBlockAttributes = LsBlockAttributes []

when = keepDelimsL . whenElt
dWhen = dropInitBlank . when

isLsBB (LsBlock _ _) = True
isLsBB _ = False
