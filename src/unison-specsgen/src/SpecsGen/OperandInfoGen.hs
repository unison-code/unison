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
module SpecsGen.OperandInfoGen (emitOperandInfo) where

import Data.Maybe
import qualified Data.Map as M
import Language.Haskell.Syntax

import SpecsGen.SimpleYaml
import SpecsGen.HsGen

data UnisonOperandInfo =
    TemporaryInfo String Integer |
    BoundInfo |
    BlockRefInfo
    deriving (Eq, Ord, Show)

emitOperandInfo targetName rcs is =
    let oi2ids = infoToIds iOperandInfo is
        rhss   = map (mkOpcRhs idToHsCon (toOperandInfoRhs rcs)) oi2ids
    in [hsModule
        (moduleName targetName "OperandInfo")
        (Just [hsExportVar "operandInfo"])
        [unisonImport, instructionDeclImport targetName,
         registerClassDeclImport targetName]
        [simpleOpcFunBind "operandInfo" rhss]]

iOperandInfo i =
    let ops      = mkOperandMap $ iOperands i
        o2d      = M.empty
        (us, ds) = iUseDefs i
        toOpInfo = map (toOperandInfo o2d ops)
        l        = iLatency i
    in (toOpInfo us, toOpInfo ds, l)

toOperandInfo o2d ops op =
  let d = fromMaybe 0 (M.lookup op o2d)
  in mkOperandInfo d (ops M.! op)

mkOperandInfo _ (YRegisterInfo _ rc (Just d)) = TemporaryInfo rc d
mkOperandInfo d (YRegisterInfo _ rc Nothing)  = TemporaryInfo rc d
mkOperandInfo _ YBlockRefInfo                 = BlockRefInfo
mkOperandInfo _ YBoundInfo                    = BoundInfo

toOperandInfoRhs rcs (uoi, doi, l) =
    let uoiTup = map (toOperandInfoExp rcs 0) uoi
        doiTup = map (toOperandInfoExp rcs l) doi
    in HsTuple [HsList uoiTup, HsList doiTup]

toOperandInfoExp _ _ BoundInfo = toHsCon "BoundInfo"
toOperandInfoExp rcs l (TemporaryInfo rc d) =
    let lat = d + l
        lf  = if lat < 0 then HsParen else id
    in HsApp (toHsCon "TemporaryInfo")
       (HsApp (HsParen (toRegClassExp rcs rc))
              (HsApp (lf (toHsInt (d + l)))
                     (toHsCon (if lat < 0 then "True" else "False"))))

toOperandInfoExp _ _ BlockRefInfo = toHsCon "BlockRefInfo"

toRegClassExp rcs rc =
    HsApp (toHsCon (regClassCon rcs rc)) (toHsCon (toOpType rc))

regClassCon (ircs, arcs) rc
  | rc `elem` ircs = "InfiniteRegisterClass"
  | rc `elem` arcs = "AbstractRegisterClass"
  | otherwise = "RegisterClass"
