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
module Unison.Tools.Extend.ExtendWithCopies (extendWithCopies) where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Control.Arrow

import Common.Util

import Unison
import Unison.Target.API
import Unison.Target.RegisterArray
import qualified Unison.Graphs.SG as SG
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.BCFG as BCFG
import qualified Unison.Graphs.Partition as P
import Unison.Analysis.CallingConvention

extendWithCopies f target =
  let cf        = copies target
      apf       = alignedPairs target
      bif       = branchInfo target
      ra        = mkRegisterArray target 0
      cst       = calleeSavedTemps f
      cg        = CG.fromFunction f
      bcfg      = BCFG.fromFunction bif f
      sg        = SG.fromFunction (Just apf) f
      fInfo     = (f, cst, cg, ra, bcfg, sg)
      id        = newId (fCode f)
      t2rs      = mkMustRegistersMap f
      (f', id') = foldWithTempIndex
                  (extendBB True virtualTemps always (cf fInfo) t2rs)
                  (f {fCode = []}, id) (fCode f)
      code''    = filterCode (not . isVirtualCopy) (fCode f')
      f''       = f' {fCode = code''}
      cg'       = CG.fromFunction f''
      bcfg'     = BCFG.fromFunction bif f''
      sg'       = SG.fromFunction (Just apf) f''
      fInfo'    = (f'', cst, cg', ra, bcfg', sg')
      t2rs'     = mkMustRegistersMap f''
      (f''', _) = foldWithTempIndex
                  (extendBB False allTemps notRCopy (cf fInfo') t2rs')
                  (f'' {fCode = []}, id') (fCode f'')
  in f'''

-- | Extends the block temporaries given by the function ft with copies
extendBB vc ft rf cf t2rs
  (ti, (f @ Function {fCode = accCode, fCongruences = cs}, id))
  b @ Block {bCode = code} =
  let (ids, itf)                = ft code
      init                      = (ti, code, [], id, t2rs)
      (ti', code', irs, id', _) = foldl (extendTemporary vc itf rf cf) init ids
      cs'                       = updateSame cs irs
  in (ti', (f {fCode = accCode ++ [b {bCode = code'}], fCongruences = cs'}, id'))

-- | Gives a list of ids and a function from ids to all temporary pairs of the
-- | form (t, t)
allTemps code =
    let id2t = zip [0..] [(t, t) | t <- tUniqueOps code]
        itf  = applyTempMap (M.fromList id2t)
        ids  = fst $ unzip id2t
    in (ids, itf)

applyTempMap m id _ = m M.! id

notRCopy = not . isNonVirtualCopy

-- | Gives a list of ids and a function from ids to pairs of temporaries in
-- | virtual copies
virtualTemps code = ([oId i | i <- code, isVirtualCopy i], virtualCopyTemps)

virtualCopyTemps id = copyOps . findBy id oId

-- | Extends the given temporary pair. The tuple (src, dst) contains different
-- | temporaries only when virtual copy temporaries are extended
extendTemporary vc itf rf cf (ti, code, irs, id, t2rs) oId =
    let (src, dst) = itf oId code
        code'      = filter rf code
        d          = find (isDefiner src) code'
        us         = filter (isUser dst) code'
    in extendReferences vc cf (src, dst) d us (ti, code, irs, id, t2rs)

extendReferences _ _ _ Nothing _  out = out
extendReferences _ _ _ _       [] out = out
extendReferences vc cf (src, dst) (Just d) us (ti, code, irs, id, t2rs) =
    let rs           = mustRegisters t2rs src
        (dcs, ucs)   = cf vc src rs d us
        t'           = if null dcs then src else mkTemp ti
        extDefOut    = extend vc undefT src after (ti, code, [], id, t2rs) (d, dcs)
        (ti', code',
         irs', id',
         t2rs')      = foldl (extend vc dst t' before) extDefOut (zip us ucs)
    in (ti', code', irs ++ irs', id', t2rs')

-- | Updates the same tuples according to the irs tuples
updateSame same irs =
  let s     = M.fromList [(undoPreAssign t, undoPreAssign t')
                         | (i, (t, t')) <- irs, isOut i]
      same' = map (first (applyMap s)) same
  in same'

updateTemps (firstT, newT) oprToExtend code =
    let tMap   = M.fromList [(tId firstT, tId newT)]
        isInst = isId (oId oprToExtend)
        code'  = mapIf isInst (mapToModelOperand (applyTempIdMap tMap)) code
    in code'

-- | Extends the code according to firstT, prevT and pos
extend _ firstT prevT _ (ti, code, irs, id, t2rs) (oprToExtend, []) =
    let r      = (firstT, prevT)
        code'  = updateTemps r oprToExtend code
        t2rs'  = replaceTemp t2rs r
    in (ti, code', irs ++ [(oprToExtend, r)], id, t2rs')

extend vc firstT prevT pos (ti, code, irs, id, t2rs) (oprToExtend, insts) =
    let newT   = mkTemp ti
        ti'    = ti + 1
        id'    = id + 1
        copy   = mkCopy id insts (undoPreAssign prevT) [] (undoPreAssign newT) []
        copy'  = mapToAttrVirtualCopy (const vc) copy
        r      = (firstT, newT)
        code'  = updateTemps r oprToExtend code
        code'' = insertWhen pos (isIdOf oprToExtend) [copy'] code'
    in (ti', code'', irs ++ [(oprToExtend, r)], id', t2rs)

undefT = mkTemp (-1)

-- | Gives a map from a temporary to a list of registers such that either that
-- temporary or a congruent one must be placed in the register(s)
mkMustRegistersMap f @ Function {fCode = code} =
    let must   = preAssignments code
        mMap   = combineMustsByTemp must
        sg     = SG.fromFunction Nothing f
        tsToRs = [(ts, nubMaybes [M.lookup (mkTemp t) mMap | t <- ts])
                  | ts <- P.toList sg]
    in M.fromList (concat [[(mkTemp t, rs) | t <- ts] | (ts, rs) <- tsToRs])

nubMaybes :: Eq r => [Maybe [Operand r]] -> [Operand r]
nubMaybes = nub . concat . catMaybes

mustRegisters t2rs t = map (rTargetReg . regId) $ M.findWithDefault [] t t2rs

combineMustsByTemp :: Ord r => [(t1, Operand r, Operand r)] ->
                      M.Map (Operand r) [Operand r]
combineMustsByTemp = M.fromListWith combineMust . mustToAList

mustToAList must = [(t, [r]) | (_, t, r) <- must]

combineMust rs1 = nub . (rs1 ++)

replaceTemp t2rs (oldT, newT)
  | oldT == undefT = t2rs
  | otherwise =
    let oldRs  = t2rs M.! oldT
        t2rs'  = M.delete oldT t2rs
        t2rs'' = M.update (Just . nub . (++) oldRs) newT t2rs'
    in t2rs''
