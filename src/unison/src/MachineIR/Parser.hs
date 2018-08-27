{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Support to parse MIR functions (see <http://llvm.org/docs/MIRLangRef.html>).

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, CPP, NoMonomorphismRestriction #-}
module MachineIR.Parser
       (MachineIR.Parser.parse, splitDocs, combineDocs, mirOperand, mirFI,
        mirJTI, mirConstantPoolIndex) where

import Data.Maybe
import Data.Char
import Data.List.Split hiding (sepBy, endBy)
import Data.Yaml
#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>),(<*>))
#endif
import Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

import Common.Util hiding (between)
import MachineIR.Base
import MachineIR.Constructors
import MachineIR.Util

parse :: Read i => Read r => MachineIRVersion -> String -> [MachineFunction i r]
parse version input = map (parseFunction version) (splitDocs version input)

splitDocs :: MachineIRVersion -> String -> [(String, String)]
splitDocs LLVM5 input =
  -- A MIR file contains copies of the raw IR interleaved with the MIR of
  -- each function in the module.
  let docs = split onDocumentEnd input
  in [(rawIR, rawMIR) | [rawIR, rawMIR] <- chunksOf 2 docs]
splitDocs LLVM6 input =
  -- A MIR file contains a first document with the raw IR followed by a
  -- document with the MIR of each function in the module.
  case split onDocumentEnd input of
   [] -> []
   rawIR : rawMIRs -> [(rawIR, rawMIR) | rawMIR <- rawMIRs]

combineDocs :: MachineIRVersion -> [(String, String)] -> String
combineDocs LLVM5 outputs =
  concat [rawIR ++ rawMIR | (rawIR, rawMIR) <- outputs]
combineDocs LLVM6 ((rawIR, rawMIR) : outputs) =
  concat ([rawIR, rawMIR] ++ map snd outputs)

parseFunction :: Read i => Read r => MachineIRVersion -> (String, String) ->
                 MachineFunction i r
parseFunction v (rawIR, rawMIR) =
  let ir  = decodeYaml rawIR  :: String
      mir = decodeYaml rawMIR :: MIRFunction
      mrs = fmap toMachineFunctionPropertyRegisters (registers mir)
      mjt = fmap toMachineFunctionPropertyJumpTable (jumpTable mir)
      mfs = fmap toMachineFunctionPropertyFixedStack (fixedStack mir)
      ms  = fmap toMachineFunctionPropertyStack (stack mir)
      mcs = fmap toMachineFunctionPropertyConstants (constants mir)
      mf  = case P.parse (mirBody v) "" (body mir) of
        Left e -> error ("error parsing body of '"
                         ++ name mir ++ "':\n" ++ show e)
        Right mf -> mf {mfName = name mir, mfIR = ir,
                        mfProperties = maybeToList mrs ++ maybeToList mjt ++
                                       maybeToList mfs ++ maybeToList ms ++
                                       maybeToList mcs}
      mf1 = mapToMachineInstruction readTargetOpcode mf
      mf2 = mapToMachineInstruction (mapToMachineOperand readOperand) mf1
  in mf2

data MIRFunction = MIRFunction {
  name :: String,
  registers :: Maybe [MIRRegisterObject],
  fixedStack :: Maybe [MIRStackObject],
  stack :: Maybe [MIRStackObject],
  constants :: Maybe [MIRConstantObject],
  jumpTable :: Maybe MIRJumpTable,
  body :: String
} deriving Show

instance FromJSON MIRFunction where
    parseJSON (Object v) =
      MIRFunction <$>
      (v .: "name") <*>
      (v .:? "registers") <*>
      (v .:? "fixedStack") <*>
      (v .:? "stack") <*>
      (v .:? "constants") <*>
      (v .:? "jumpTable") <*>
      (v .: "body")
    parseJSON _ = error "Can't parse MIRFunction from YAML"

data MIRStackObject = MIRStackObject {
  sId                  :: Integer,
  sOffset              :: Integer,
  sSize                :: Maybe Integer,
  sAlignment           :: Integer,
  sCalleeSavedRegister :: Maybe String
} deriving Show

instance FromJSON MIRStackObject where
    parseJSON (Object v) =
      MIRStackObject <$>
      (v .:  "id") <*>
      (v .:  "offset") <*>
      (v .:? "size") <*>
      (v .:  "alignment") <*>
      (v .:? "callee-saved-register")
    parseJSON _ = error "Can't parse MIRStackObject from YAML"

data MIRRegisterObject = MIRRegisterObject {
  rId    :: Integer,
  rClass  :: String
} deriving Show

instance FromJSON MIRRegisterObject where
    parseJSON (Object v) =
      MIRRegisterObject <$>
      (v .:  "id") <*>
      (v .:  "class")
    parseJSON _ = error "Can't parse MIRRegisterObject from YAML"

data MIRConstantObject = MIRConstantObject {
  cId    :: Integer,
  cValue :: String,
  cAli   :: Integer
} deriving Show

instance FromJSON MIRConstantObject where
    parseJSON (Object v) =
      MIRConstantObject <$>
      (v .:  "id") <*>
      (v .:  "value") <*>
      (v .:  "alignment")
    parseJSON _ = error "Can't parse MIRConstantObject from YAML"

data MIRJumpTable = MIRJumpTable {
  kind    :: String,
  entries :: [MIRJumpTableEntry]
} deriving Show

instance FromJSON MIRJumpTable where
    parseJSON (Object v) =
      MIRJumpTable <$>
      (v .: "kind") <*>
      (v .: "entries")
    parseJSON _ = error "Can't parse MIRJumpTable from YAML"

data MIRJumpTableEntry = MIRJumpTableEntry {
  jtId     :: Integer,
  jtBlocks :: [String]
} deriving Show

instance FromJSON MIRJumpTableEntry where
    parseJSON (Object v) =
      MIRJumpTableEntry <$>
      (v .: "id") <*>
      (v .: "blocks")
    parseJSON _ = error "Can't parse MIRJumpTableEntry from YAML"

mirBody v =
  do blocks <- many (mirBlock v)
     eof
     return (mkMachineFunction "" [] blocks "")

mirBlock v =
  do id <- mirBlockId
     whiteSpace
     attrs <- option [] (parens (mirBlockAttribute `sepBy` comma))
     char ':'
     eol
     succs <- optionMaybe (try mirBlockSuccessors)
     entry <- optionMaybe (try $ mirBlockLiveIns v)
     ret   <- optionMaybe (try $ mirBlockLiveOuts v)
     exit  <- optionMaybe (try mirBlockExit)
     many eol
     instructions <- many (mirInstruction v)
     many eol
     return (mkMachineBlock id
             (concatAttributes attrs succs)
             (concatInstructions id entry ret exit instructions))

mirBlockId =
  do string "bb."
     id <- decimal
     optional mirBlockName
     return id

mirBlockName =
  do char '.'
     name <- many1 alphaNumDashDotUnderscore
     return name

mirBlockAttribute =
  try mirBlockAttributeAlign <|>
  try mirBlockAttributeAddressTaken <|>
  try mirBlockAttributeName <|>
  try mirBlockAttributeFreq

mirBlockAttributeAlign =
  do string "align"
     whiteSpace
     decimal
     return Nothing

mirBlockAttributeAddressTaken =
  do string "address-taken"
     return Nothing

mirBlockAttributeName =
  do char '%'
     many1 alphaNumDashDotUnderscore
     return Nothing

mirBlockAttributeFreq =
  do string "freq"
     whiteSpace
     f <- decimal
     return (Just (mkMachineBlockPropertyFreq f))

mirBlockSuccessors =
  do whiteSpaces 2
     string "successors:"
     whiteSpace
     bs <- mirBlockSuccessor `sepBy` comma
     eol
     return (mkMachineBlockPropertySuccs bs)

mirBlockSuccessor =
  do mbr <- mirBlockRef
     prob <- parens decimal
     return (mbrId mbr, prob)

mirBlockLiveIns v =
  do whiteSpaces 2
     string "liveins:"
     whiteSpace
     liveIns <- mirOperand v `sepBy` comma
     eol
     return liveIns

mirBlockLiveOuts v =
  do whiteSpaces 2
     string "liveouts:"
     whiteSpace
     liveOuts <- mirOperand v `sepBy` comma
     eol
     return liveOuts

mirBlockExit =
  do whiteSpaces 2
     string "exit"
     eol
     return ()

mirInstruction v = try (mirBundle v) <|> try (mirSingle v 2)

mirBundle v =
  do whiteSpaces 2
     optional (try frameAnnotation)
     opcode <- mirOpcode
     whiteSpace
     us <- mirOperand v `sepBy` comma
     whiteSpace
     char '{'
     eol
     instructions <- many (try (mirSingle v 4))
     whiteSpaces 2
     char '}'
     eol
     return (mkMachineBundleWithHeader
             (mkMachineSingle opcode [] us) instructions)

mirSingle v n =
  do whiteSpaces n
     ds <- option [] (mirDefOperands v)
     optional (try frameAnnotation)
     opcode <- mirOpcode
     whiteSpace
     us <- mirOperand v `sepBy` comma
     whiteSpace
     optional mirMemOperands
     eol
     return (mkMachineSingle opcode [] (ds ++ us))

frameAnnotation = try frameSetup <|> try frameDestroy
frameSetup   = string "frame-setup "
frameDestroy = string "frame-destroy "

mirOpcode = try mirVirtualOpcode <|> mirTargetOpcode

mirVirtualOpcode =
  try (mirVOpc ("PHI", PHI)) <|>
  try (mirVOpc ("COPY", COPY)) <|>
  try (mirVOpc ("ENTRY", ENTRY)) <|>
  try (mirVOpc ("RETURN", RETURN)) <|>
  try (mirVOpc ("EXIT", EXIT)) <|>
  try (mirVOpc ("EXTRACT_SUBREG", EXTRACT_SUBREG)) <|>
  try (mirVOpc ("LOW", LOW)) <|>
  try (mirVOpc ("HIGH", HIGH)) <|>
  try (mirVOpc ("SPLIT2", SPLIT2)) <|>
  try (mirVOpc ("SPLIT4", SPLIT4)) <|>
  try (mirVOpc ("IMPLICIT_DEF", IMPLICIT_DEF)) <|>
  try (mirVOpc ("INSERT_SUBREG", INSERT_SUBREG)) <|>
  try (mirVOpc ("REG_SEQUENCE", REG_SEQUENCE)) <|>
  try (mirVOpc ("SUBREG_TO_REG", SUBREG_TO_REG)) <|>
  try (mirVOpc ("COMBINE", COMBINE)) <|>
  try (mirVOpc ("ADJCALLSTACKUP", ADJCALLSTACKUP)) <|>
  try (mirVOpc ("ADJCALLSTACKUP32", ADJCALLSTACKUP32)) <|>
  try (mirVOpc ("ADJCALLSTACKUP64", ADJCALLSTACKUP64)) <|>
  try (mirVOpc ("ADJCALLSTACKDOWN", ADJCALLSTACKDOWN)) <|>
  try (mirVOpc ("ADJCALLSTACKDOWN32", ADJCALLSTACKDOWN32)) <|>
  try (mirVOpc ("ADJCALLSTACKDOWN64", ADJCALLSTACKDOWN64)) <|>
  try (mirVOpc ("ANNOTATION_LABEL", ANNOTATION_LABEL)) <|>
  try (mirVOpc ("CFI_INSTRUCTION", CFI_INSTRUCTION)) <|>
  try (mirVOpc ("EH_LABEL", EH_LABEL)) <|>
  try (mirVOpc ("BLOCK_MARKER", BLOCK_MARKER)) <|>
  try (mirVOpc ("BUNDLE", BUNDLE))

mirVOpc (name, opc) =
  do string name
     notFollowedBy alphaNumDashDotUnderscore
     return (mkMachineVirtualOpc opc)

mirTargetOpcode =
  do opc <-many1 alphaNumDashDotUnderscore
     return (mkMachineVirtualOpc (FREE_OPCODE opc))

mirDefOperands v =
  do ds <- mirOperand v `sepBy` comma
     string " = "
     return ds

mirOperand v =
  do optional (try mirTargetFlags)
     whiteSpace
     op <- mirActualOperand v
     return op

mirTargetFlags = string "target-flags(<unknown>)"

mirActualOperand v =
  try mirConstantPoolIndex <|>
  try mirBlockRef <|>
  try mirJTI <|>
  try mirFI <|>
  try mirMFS <|>
  try mirSubRegIndex <|>
  try (mirReg v) <|>
  try mirImm <|>
  try mirNullReg <|>
  try mirGlobalAdress <|>
  try (mirExternalSymbol v) <|>
  try mirMemPartition <|>
  try mirProperty <|>
  try mirBlockFreq <|>
  try mirDebugLocation <|>
  try mirMCSymbol <|>
  try mirFPImm <|>
  try mirRawFPImm <|>
  try (mirCFIDef v) <|>
  try mirCFIDefOffset <|>
  try (mirCFIDefReg v) <|>
  try (mirCFIOffset v) <|>
  try mirCFIAdjustCfaOffset <|>
  try mirRegMask

mirConstantPoolIndex =
  do string "%const."
     idx <- many1 alphaNumDashDotUnderscore
     return (mkMachineConstantPoolIndex idx)

mirSubRegIndex =
  do string "%subreg."
     idx <- many1 alphaNumDashDotUnderscore
     return (mkMachineSubRegIndex idx)

mirReg v =
  do states <- mirRegFlag `endBy` whiteSpace
     mirRegPrefix v
     reg <- mirSpecificReg v (catMaybes states)
     return reg

mirRegPrefix LLVM5 = char '%'
mirRegPrefix LLVM6 = P.oneOf "%$"

mirExternalSymbolPrefix LLVM5 = char '$'
mirExternalSymbolPrefix LLVM6 = char '&'

mirJTI =
  do string "%jump-table."
     idx <- decimal
     return (mkMachineJumpTableIndex idx)

mirFI = try fixedMirFI <|> try varMirFI

fixedMirFI =
  do string "%fixed-stack."
     idx <- decimal
     return (mkMachineFrameIndex idx True 0)

varMirFI =
  do string "%stack."
     idx <- decimal
     optional (try fIName)
     return (mkMachineFrameIndex idx False 0)

mirMFS =
  do string "%frame-size"
     return mkMachineFrameSize

fIName =
  do char '.'
     string "<unnamed alloca>" <|> many1 alphaNumDashDotUnderscore
     return ()

mirSpecificReg v states =
  try (mirVirtualReg v states) <|> (mirMachineReg states)

mirVirtualReg v states =
  do id <- decimal
     srid <- optionMaybe (mirSuffixSubRegIndex v)
     optional mirVirtualRegClass
     td <- optionMaybe mirTiedDef
     return (case (srid, td) of
               (Nothing, td) -> mkMachineTemp id states td
               (Just idx, Nothing) -> mkMachineSubTemp id idx)

mirSuffixSubRegIndex v =
  do char (case v of
            LLVM5 -> ':'
            LLVM6 -> '.')
     idx <- many1 alphaNumDashDotUnderscore
     return idx

mirVirtualRegClass =
  do char ':'
     idx <- many1 alphaNumDashDotUnderscore
     return idx

mirTiedDef =
  do string "(tied-def "
     id <- decimal
     string ")"
     return id

mirMachineReg states = try mirLongNullReg <|> (mirMachineFreeReg states)

mirLongNullReg =
  do string "noreg"
     return mkMachineNullReg

mirMachineFreeReg states =
  do name <- many alphaNumDashDotUnderscore
     optional (try mirTiedDef)
     return (mkMachineFreeReg name states)

mirRegFlag =
    try mirRegImplicitDefine <|>
    try mirRegImplicit <|>
    try mirRegUndef <|>
    try mirRegOther

mirRegImplicitDefine =
  do string "implicit-def"
     return (Just mkMachineRegImplicitDefine)

mirRegImplicit =
  do string "implicit"
     return (Just mkMachineRegImplicit)

mirRegUndef =
  do string "undef"
     return (Just mkMachineRegUndef)

mirRegOther =
  do try (string "def") <|>
       try (string "dead") <|>
       try (string "killed") <|>
       try (string "internal") <|>
       try (string "early-clobber") <|>
       try (string "debug-use") <|>
       try (string "renamable")
     return Nothing

mirImm =
  do imm <- signedDecimal
     return (mkMachineImm imm)

mirBlockRef =
  do char '%'
     id <- mirBlockId
     return (mkMachineBlockRef id)

mirNullReg =
  do char '_'
     return mkMachineNullReg

mirGlobalAdress =
  do char '@'
     (address, offset) <- mirAddress
     return (mkMachineGlobalAddress address offset)

mirAddress =
  do optional (string "\"\\")
     address <- many1 alphaNumDashDotUnderscore
     optional (char '"')
     whiteSpace
     offset <- try (option 0 mirOffset)
     return (address, offset)

mirOffset =
  do sign <- char '+' <|> char '-'
     whiteSpace
     offset <- decimal
     return (case sign of
                '-' -> offset * (-1)
                '+' -> offset)

mirExternalSymbol v =
  do mirExternalSymbolPrefix v
     sym <- many1 alphaNumDashDotUnderscore
     return (mkMachineExternal sym)

mirMemPartition =
  do (address, id) <- mirMDInt "unison-memory-partition"
     return (mkMachineMemPartition address id)

mirProperty =
  do string "<0"
     address <- hexadecimal
     string "> = !{!\"unison-property\", !\""
     pr <- many1 (noneOf "\"")
     string "\"}"
     return (mkMachineProperty address pr)

mirBlockFreq =
  do (address, freq) <- mirMDInt "unison-block-frequency"
     return (mkMachineBlockFreq address freq)

mirDebugLocation =
  do string "debug-location !"
     id <- decimal
     return (mkMachineDebugLocation id)

mirMCSymbol =
  do string "<mcsymbol "
     sym <- many1 alphaNumDashDotUnderscore
     char '>'
     return (mkMachineSymbol sym)

mirFPImm =
  do string "float"
     whiteSpace
     int <- signedDecimal
     char '.'
     fr <- decimal
     char 'e'
     exp <- mirOffset
     return (mkMachineFPImm int fr exp)

mirRawFPImm =
  do string "float"
     whiteSpace
     char '0'
     imm <- hexadecimal
     return (mkMachineRawFPImm imm)

mirCFIDef v =
  do optional mirCFIPrefix
     string "def_cfa"
     whiteSpace
     reg <- mirReg v
     string ", "
     off <- signedDecimal
     return (mkMachineCFIDef (mfrRegName reg) off)

mirCFIDefOffset =
  do optional mirCFIPrefix
     string "def_cfa_offset"
     whiteSpace
     off <- signedDecimal
     return (mkMachineCFIDefOffset off)

mirCFIDefReg v =
  do optional mirCFIPrefix
     string "def_cfa_register"
     whiteSpace
     reg <- mirReg v
     return (mkMachineCFIDefReg (mfrRegName reg))

mirCFIOffset v =
  do optional mirCFIPrefix
     string "offset"
     whiteSpace
     reg <- mirReg v
     string ", "
     off <- signedDecimal
     return (mkMachineCFIOffset (mfrRegName reg) off)

mirCFIAdjustCfaOffset =
  do optional mirCFIPrefix
     string "adjust_cfa_offset"
     whiteSpace
     off <- signedDecimal
     return (mkMachineCFIAdjustCfaOffset off)

mirRegMask =
  do string "csr_"
     name <- many1 alphaNumDashDotUnderscore
     return (mkMachineRegMask name)

mirMDInt tag =
  do string "<0"
     address <- hexadecimal
     string ("> = !{!\"" ++ tag ++ "\",")
     whiteSpace
     string "i"
     decimal
     whiteSpace
     val <- decimal
     string "}"
     return (address, val)

mirMemOperands =
  do string "::"
     ms <- many1 (noneOf "\n\r")
     return ms

-- This prefix is present only in LLVM <= 3.9
mirCFIPrefix = string ".cfi_"

whiteSpaces n = string (replicate n ' ')

eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n" <|>
      string "\r"
      <?> "eol"

lexer = T.makeTokenParser emptyDef
whiteSpace = many (char ' ')
decimal = T.decimal lexer
hexadecimal = T.hexadecimal lexer
comma = T.comma lexer

parens = between (char '(') (char ')')

alphaNumDashDotUnderscore =
  satisfy isAlphaNumDashDotUnderscore <?> "letter, digit, dash, dot or underscore symbol"

isAlphaNumDashDotUnderscore '-' = True
isAlphaNumDashDotUnderscore '_' = True
isAlphaNumDashDotUnderscore '.' = True
isAlphaNumDashDotUnderscore c = isAlphaNum c

signedDecimal =
  do s <- option 1 sign
     dec <- decimal
     return (s * dec)

sign =
  do char '-'
     return (-1)

concatAttributes attrs succs = maybeToList succs ++ catMaybes attrs

concatInstructions id entry ret exit instructions =
  let en = case id of
        0 ->
          let es = case entry of
                (Just es') -> es'
                Nothing -> []
          in [mkMachineSingle (mkMachineVirtualOpc ENTRY) [] es]
        _ -> []
      r  = case ret of
        (Just rs) -> [mkMachineSingle (mkMachineVirtualOpc RETURN) [] rs]
        Nothing -> []
      ex = case exit of
        (Just ()) -> [mkMachineSingle (mkMachineVirtualOpc EXIT) [] []]
        Nothing -> []
   in en ++ instructions ++ r ++ ex

toMachineFunctionPropertyRegisters rs =
    mkMachineFunctionPropertyRegisters (map toMachineFunctionRegister rs)

toMachineFunctionRegister MIRRegisterObject {rId = id, rClass = cl} = (id, cl)

toMachineFunctionPropertyFixedStack fso =
    mkMachineFunctionPropertyFixedFrame (map toMachineFrameObjectInfo fso)

toMachineFunctionPropertyStack fso =
    mkMachineFunctionPropertyFrame (map toMachineFrameObjectInfo fso)

toMachineFrameObjectInfo
  MIRStackObject {sId = id, sOffset = off, sSize = s, sAlignment = ali,
                  sCalleeSavedRegister = csreg} =
    mkMachineFrameObjectInfo id off s ali (maybeRead csreg)

toMachineFunctionPropertyConstants rs =
    mkMachineFunctionPropertyConstants (map toMachineFunctionConstant rs)

toMachineFunctionConstant MIRConstantObject {cId = id, cValue = v, cAli = a} =
  (id, v, a)

maybeRead Nothing   = Nothing
maybeRead (Just "") = Nothing
maybeRead (Just r)  = Just (read $ tail r)

toMachineFunctionPropertyJumpTable MIRJumpTable {kind = k, entries = es} =
  let mes = map toMachineJumpTableEntry es
  in mkMachineFunctionPropertyJumpTable k mes

toMachineJumpTableEntry MIRJumpTableEntry {jtId = id, jtBlocks = bs} =
  mkMachineJumpTableEntry id (map parseMirBlockRef bs)

parseMirBlockRef b =
  case P.parse mirBlockRef "" b of
    Left e -> error ("error parsing block reference in jump table:\n" ++ show e)
    Right br -> br

readTargetOpcode mi @ MachineSingle {
                       msOpcode = MachineVirtualOpc (FREE_OPCODE opc)} =
  mi {msOpcode = mkMachineTargetOpc (read opc)}
readTargetOpcode mi = mi

readOperand (MachineFreeReg name states) =
  mkMachineCompleteReg (read name) states
readOperand mo = mo

mkMachineBundleWithHeader
  MachineSingle {msOpcode = MachineVirtualOpc BUNDLE} instructions =
    mkMachineBundle instructions
mkMachineBundleWithHeader instruction instructions =
  MachineBundle False (instruction : instructions)
