{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Generic functions used through the entire package.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Common.Util (
        -- * List functions
        maybeMax,
        lookupBy,
        findBy,
        toSingleton,
        fromSingleton,
        indexOf,
        expand,
        pairs,
        insertAt,
        slice,
        between,
        -- * Map functions
        applyMap,
        fromListMult,
        mapIf,
        applyIf,
        mapAppend,
        inverseMap,
        -- * Tuple functions
        mapTuple,
        mapTriple,
        -- * Pretty printing
        cs,
        csWith,
        lineStyle,
        st,
        fill,
        wsWidth,
        wsString,
        lineWidth,
        newLine,
        showInLines,
        -- * Debugging
        traceIf,
        -- * I/O
        strictReadFile,
        maybeStrictReadFile,
        -- * Other functions
        dropPrefix,
        dropSuffix,
        maybeEval,
        always,
        none,
        maybeNothing,
        fixpointWith,
        fixpoint,
        combineAdd,
        onDocumentEnd,
        decodeYaml,
        encodeYaml,
        ShowSimple (..)
       )
       where

import Data.List
import Data.Tuple
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow
import Text.PrettyPrint
import Debug.Trace
import Data.List.Split
import Data.Yaml
import qualified Data.ByteString.Char8 as B8
import System.IO

maybeMax v [] = v
maybeMax _ l  = maximum l

lookupBy e f = find (\a -> f a == e)
findBy e f = fromJust . lookupBy e f

toSingleton a = [a]
fromSingleton [a] = a

indexOf e = toInteger . fromJust . elemIndex e

expand :: (a -> [[a]]) -> [[a]] -> [[a]]
expand _ [] = []
expand f (l:ls) =
    let fl       = foldl (\l0 e -> merge l0 (f e)) [] l
        (l':ls') = merge ([]:ls) fl
    in l':expand f ls'

merge :: [[a]] -> [[a]] -> [[a]]
merge a b
    | length a == length b = zipWith (++) a b
    | length a >  length b =
        let (a1,a2) = splitAt (length b) a
        in merge a1 b ++ a2
    | otherwise = merge b a

pairs [] = []
pairs xs = zip xs (tail xs)

insertAt e n xs =
    let (ys, zs) = splitAt n xs
    in ys ++ [e] ++ zs

slice b e l = drop b $ take (e + 1) l

between f g l =
    let (_:l') = dropWhile (not . f) l
    in  takeWhile (not . g) l'

applyMap k2t k = fromMaybe k (M.lookup k k2t)

fromListMult l = M.fromListWith (++) $ map (second toSingleton) l

mapIf p f ls = map (applyIf p f) ls

applyIf p f e = if p e then f e else e

mapAppend (k, v) = M.insertWith (++) k v

inverseMap m = M.fromList (map swap $ M.toList m)

mapTuple f (i, j) = (f i, f j)

mapTriple f (i, j, k) = (f i, f j, f k)

lineWidth = 80
wsWidth = 4
newLine = "\n"

wsString = replicate wsWidth ' '

mapCS f s = punctuate (text s) . map f
csWith s f = fsep . mapCS (text . f) s
cs = csWith ","

st l = Style {mode = PageMode, lineLength = fromInteger l, ribbonsPerLine = 1}

lineStyle = Style {mode = OneLineMode, lineLength = 0, ribbonsPerLine = 1}

fill n s =
  let fillWs = n - length s
  in s ++ replicate fillWs ' '

showInLines l = unlines $ map show l

traceIf True s = trace s
traceIf False _ = id

strictReadFile f =
  do h <- openFile f ReadMode
     c <- strictHGetContents h
     return c

strictHGetContents h =
  do c <- hGetContents h
     length c `seq` return c

maybeStrictReadFile Nothing = return Nothing
maybeStrictReadFile (Just file) =
  do contents <- strictReadFile file
     return (Just contents)

dropPrefix s l = drop (length s) l

dropSuffix s l = take (length l - length s) l

maybeEval f x y
  | any isNothing [x, y] = Nothing
  | otherwise = Just $ f (fromJust x) (fromJust y)

always = const True

none f = not . any f

maybeNothing True m  = m
maybeNothing False _ = Nothing

-- | Runs the function f with the input i until a fixpoint is reached.
--   The comparison is performed by c.
fixpointWith c f i =
  let i' = f i
  in if c i' i then i' else fixpoint f i'

fixpoint f = fixpointWith (==) f

combineAdd :: Ord a => Num b => [(a, b)] -> [(a, b)]
combineAdd = M.toList . M.fromListWith (+)

onDocumentEnd = dropBlanks $ keepDelimsR $ keepDelimsL $ onSublist "...\n"

decodeYaml s =
  case decodeEither $ B8.pack s of
    (Left err) -> error err
    Right yaml -> yaml

encodeYaml y = B8.unpack $ encode y

-- | Something that can be shown in a simple way
class ShowSimple a where
    showSimple :: a -> String
