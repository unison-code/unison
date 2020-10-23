#!/usr/bin/env runhaskell

{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  rcas@acm.org

Script to pretty-print Hexagon assembly code.

-}
{-
Main authors:
  Roberto Castaneda Lozano <rcas@acm.org>

This file is part of Unison, see http://unison-code.github.io
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Data.List
import Data.Maybe
import System.Console.CmdArgs
import Text.ParserCombinators.Parsec
import Data.List.Split hiding (endBy)

data Pack = Pack {inFile :: String} deriving (Data, Typeable, Show)

packArgs = cmdArgsMode $ Pack
    {
     inFile  = "" &= argPos 1 &= typFile
    }

main =
    do Pack{..} <- cmdArgsRun packArgs
       inAsm <- readFile inFile
       let asm = parseFunction inAsm
       putStr (show asm)

parseFunction asm =
  case Text.ParserCombinators.Parsec.parse function "" asm of
    Left e -> error ("error parsing input:\n" ++ show e)
    Right (Function sts) -> Function (bundle (filter (not . isEmpty) sts))

data Function = Function [Statement] deriving (Eq, Ord)

data Statement =
    BundleStart |
    BundleEnd String |
    Single String |
    Bundle [Statement] String
    deriving (Eq, Ord)

function =
  do body <- line `endBy` eol
     return (Function body)

line = try bundleStart <|> try bundleEnd <|> try ident <|> single

bundleStart =
  do string "\t{"
     return BundleStart

bundleEnd =
  do string "\t}"
     suffix <- many (noneOf "\n")
     return (BundleEnd suffix)

ident =
  do string "\t.ident\t"
     line <- many (noneOf "\n")
     return (Single (".ident\t" ++ line))

single =
  do line <- many (noneOf "\n/")
     return (Single (clear line))

eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n" <|>
      string "\r" <|>
      comment
      <?> "eol"

comment =
  do string "//"
     many (noneOf "\n/")
     return ""

bundle asm = doBundle Nothing asm

doBundle Nothing (BundleStart : asm) =
    doBundle (Just []) asm
doBundle (Just sts) ((BundleEnd suffix) : asm) =
    (Bundle sts suffix) : doBundle Nothing asm
doBundle (Just sts) ((Single s) : asm) =
    doBundle (Just (sts ++ [Single s])) asm
doBundle Nothing ((Single s) : asm) =
    Single s : doBundle Nothing asm
doBundle _ [] = []

instance Show Function where
  show (Function sts) =
    concat [show st ++ "\n" | st <- sts]

instance Show Statement where
  show (Single st) = showSingle False (Single st)
  show (Bundle [st] _) = showSingle False st
  show (Bundle sts suffix) =
      let sts' = intersperse (Single "; ") sts
      in "\t{" ++ concatMap (showSingle True) sts' ++ "}" ++ suffix

showSingle False (Single ('.' : st)) = "\t." ++ st
showSingle False (Single st) = "\t" ++ st
showSingle True (Single st)  = st

clear s = strip (delete '\t' s)

fill n s =
  let fillWs = n - length s
  in s ++ replicate fillWs ' '

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

isEmpty (Single "") = True
isEmpty (Single s)  = ".file" `isPrefixOf` s
isEmpty _ = False
