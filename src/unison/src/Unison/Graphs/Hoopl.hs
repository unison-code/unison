{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Hoopl block control-flow graph of a function.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
{-# LANGUAGE ScopedTypeVariables, GADTs, NoMonomorphismRestriction, CPP #-}
module Unison.Graphs.Hoopl
    (HOperation (..), toHGraph, toBody, hLabelToLabel, labelToHLabel,
     toHOprGraph) where

#if (defined(__GLASGOW_HASKELL__)) && (__GLASGOW_HASKELL__ >= 710)
import Prelude hiding ((<*>))
#endif
import Data.List
import Compiler.Hoopl
import Compiler.Hoopl.Internals (uniqueToLbl, lblToUnique)
import Data.Graph.Inductive

import Unison.Base
import Unison.Util hiding (successors)
import Unison.Constructors
import Unison.Instances()

data HOperation i r e x where
    HIn     :: Label -> BlockOperation i r -> HOperation i r C O
    HMiddle :: BlockOperation i r -> HOperation i r O O
    HOut    :: BlockOperation i r -> [Label] -> HOperation i r O C

instance (Show i, Show r) => Show (HOperation i r e x) where
  show (HIn l i) = show l ++ ": " ++ show i ++ "\n"
  show (HMiddle i)   = show i
  show (HOut i s) = show i ++ " -- succ: " ++ show s

instance Show i => NonLocal (HOperation i r) where
    entryLabel (HIn l _) = l
    successors (HOut _ s) = s

hLabelToLabel = toInteger . lblToUnique
labelToHLabel = uniqueToLbl . fromInteger

toBody (GMany _ body _) = body

-- | Function to Hoopl graph
toHGraph bif Function {fCode = code} =
    let (bid, oid) = (newBlockIndex code, newOprIndex $ flatten code)
        sf = blockSucc bif (bLab $ last code)
        g  = foldl (|*><*|) emptyClosedGraph (map (toHBlock bid sf) code)
        -- | This is a work-around to get the facts from the last iteration in
        -- forward analysis.
        g' = g |*><*| mkSink bid oid
    in g'

-- | Block to Hoopl block
toHBlock bid sf bl @ (Block {bLab = l, bCode = code}) =
    let (sr, b, sk) = decompose code
        hSuccs      = map labelToHLabel (sort $ sf bl) ++ [labelToHLabel bid]
        src         = HIn (labelToHLabel l) sr
        body        = map HMiddle b
        snk         = HOut sk hSuccs
    in mkFirst src <*> mkMiddles body <*> mkLast snk


mkSink bid oid =
    let src         = HIn  (labelToHLabel bid) (mkIn oid [])
        snk         = HOut (mkOut (oid + 1) []) []
    in mkFirst src <*> mkMiddles [] <*> mkLast snk

decompose ::  [t] -> (t, [t], t)
decompose code = (head code, init $ tail code, last code)

-- | Function to Hoopl operation graph
toHOprGraph icfg =
    let bs = map (toHOprBlock icfg) (labNodes icfg)
        g  = foldl (|*><*|) emptyClosedGraph bs
    in g

-- | Operation to Hoopl block
toHOprBlock icfg (l, (_, o)) =
    let n2l    = labelToHLabel . toInteger
        hSuccs = map n2l $ suc icfg l
        src    = HIn (n2l l) (mkIn (-1) [])
        snk    = HOut (mkOut (-1) []) hSuccs
    in mkFirst src <*> mkMiddles [HMiddle o] <*> mkLast snk
