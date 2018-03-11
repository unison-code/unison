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
module SpecsGen.HsGen
    (
     constantExtendedOperation,
     extendRemats,
     expand,
     promote,
     update,
     makeBound,
     dummySrcLoc,
     name,
     infoToIds,
     opcOpsFunBind,
     targetInstructionCon,
     simpleOpcFunBind,
     simpleFun,
     constantFun,
     simpleErrorRhs,
     hsModule,
     hsExportDataType,
     hsExportVar,
     unisonImport,
     instructionDeclImport,
     registerDeclImport,
     itineraryDeclImport,
     registerClassDeclImport,
     hsFun,
     hsDataDecl,
     hsItinDecl,
     hsInstDecl,
     opcPVar,
     mkOpcRhs,
     toHsStr,
     toHsVar,
     toHsInt,
     toHsCon,
     toHsPStr,
     toHsPVar,
     toHsList,
     opToHsVar,
     idToHsCon,
     redefNameIfIn,
     renameRedefs,
     mkOperandMap,
     toVarName,
     toRedefName,
     toVarList,
     toOpType,
     toConstantExtendedOp,
     isConstantExtendedOp,
     moduleName
    )
    where

import Data.List
import Data.Char
import qualified Data.Map as M
import Language.Haskell.Syntax
import qualified Data.Text as T
import Control.Arrow
import Data.Function

import SpecsGen.SimpleYaml

constantExtendedOperation i
  | isConstantExtendable i =
    let oldId  = YString (oId i)
        newId  = YString $ toConstantExtendedOp $ yString oldId
        i'     = yApply (yReplaceString oldId newId) i
    in Just i'
  | otherwise = Nothing

isConstantExtendable i =
  (iType i == "copy") ||
  (any (isConstantExtendableOp . simplifyOperand) $ iOperands i)

isConstantExtendableOp (_, YBoundInfo) = True
isConstantExtendableOp (_, YBlockRefInfo) = True
isConstantExtendableOp _ = False

extendRemats is remat = concatMap (extendRemat (idMap is)) remat

extendRemat id2i r =
  let id  = yFetchStr "id" r
      hc  = yFetchStr "home-class" r
      ic  = yFetchStr "infinite-class" r
      suf = case yFetch "suffix" r of
             YString s -> s
             YNil -> ""
      i   = case M.lookup id id2i of
                Just i' -> i'
                Nothing ->
                  error ("Rematerializable instruction \'" ++ id ++ "\' does not exist")
      d   = sourceVersion id suf (hc, ic) i
      dc  = dematVersion id suf (hc, ic)
      rc  = rematVersion id suf (hc, ic)
      ris = map (maybeExpandInstr id2i) [d, dc, rc]
  in case yFetch "versions" r of
      (YSeq versions) -> [expandVersion ri v | ri <- ris, v <- versions]
      YNil -> ris

expandVersion i (YString "base") = i
expandVersion i (YMap [(YString suf, YSeq [YMap fields])]) =
  let id = oId i
      v  = YMap ([(YString "id", YString (id ++ suf))] ++
                 [(YString "parent", YString id)] ++
                 fields)
      i' = maybeExpandInstr (idMap [i]) v
  in i'

sourceVersion id suf (_, ic) i =
  let (_, [d]) = iUseDefs i
      u =  [(YString "id", YString (id ++ "_source" ++ suf))] ++
           [(YString "parent", YString id)] ++
           noUsages ++ noEffects ++
           [(YString "new-operands",
             YSeq [YMap [(YString d, YSeq [YString "register", YString "def",
                                           YString ic, YString "-1"])]])]
  in YMap u

dematVersion id suf (hc, ic) =
  YMap ([(YString "id", YString (id ++ "_demat" ++ suf))] ++
        [(YString "parent", YString id)] ++
        noUsages ++ noEffects ++ copy ++ copyOperands hc ic (Just "-1"))

rematVersion id suf (hc, ic) =
  YMap ([(YString "id", YString (id ++ "_remat" ++ suf))] ++
        [(YString "parent", YString id)] ++
        copy ++ copyOperands ic hc Nothing)

noUsages = [(YString "itinerary", YString "NoItinerary"),
            (YString "size", YString "0")]

noEffects = [(YString "affects",     YNil),
             (YString "affected-by", YNil)]

copy = [(YString "type", YString "copy")]

copyOperands s d dl =
  [(YString "uses",     YSeq [YString "src"]),
   (YString "defines",  YSeq [YString "dst"]),
   (YString "operands", YSeq [regOperand "src" "use" s Nothing,
                              regOperand "dst" "def" d dl])]

regOperand name typ rclass deflat =
  let lat = case deflat of
             Just l  -> [YString l]
             Nothing -> []
  in YMap [(YString name,
            YSeq ([YString "register", YString typ, YString rclass] ++ lat))]

idMap is = M.fromList $ map (\i -> (oId i, i)) is

expand is = map (maybeExpandInstr (idMap is)) is

maybeExpandInstr id2i i =
  case iParent i of
   Nothing -> i
   Just p ->
     let i1 = case M.lookup p id2i of
               Just i1' -> i1'
               Nothing ->
                 error ("Parent instruction \'" ++ p ++ "\' does not exist")
         i2 = foldl replaceField i1 (yMap i)
         i3 = deleteField (YString "parent") i2
         i4 = addField (YString "parent", YString p) i3
         i5 = maybeExpandOperands (yLookup "new-operands" i) i4
     in i5

maybeExpandOperands (Just (YSeq new)) i =
  let operands = nubBy (equaling operandName) $ new ++ iOperands i
      i1       = replaceField i (YString "operands", YSeq operands)
      uses     = nub $ iUses i ++
                       [YString (operandName o) | o <- new, isUse o]
      i2       = replaceField i1 (YString "uses", YSeq uses)
      defines  = nub $ iDefines i ++
                       [YString (operandName o) | o <- new, isDef o]
      i3       = replaceField i2 (YString "defines", YSeq defines)
  in i3
maybeExpandOperands Nothing i = i

equaling = ((==) `on`)

operandName (YMap [(YString name, _)]) = name

isUse (YMap [(_,YSeq (_:YString ud:_:_))]) = ud `elem` ["use", "usedef"]
isUse _ = False

isDef (YMap [(_,YSeq (_:YString ud:_:_))]) = ud `elem` ["def", "usedef"]
isDef _ = False

replaceField i (YString "parent", _) = i
replaceField (YMap fs) f = YMap (map (replace f) fs)

replace (YString newk, newv) (YString oldk, oldv)
  | newk == oldk = (YString newk, newv)
  | otherwise = (YString oldk, oldv)

addField f (YMap fs) = YMap (fs ++ [f])

deleteField df (YMap fs) = YMap (filter (not . isField df) fs)

isField f (f', _) = f == f'

promote effs i =
    let rs    = sideEffects $ iAffectedBy i
        ws    = if iType i `elem` ["call", "branch"]
                then [] -- calls and branches do not have definition operands
                else sideEffects $ iAffects i
        i1    = foldl (explicateEffect (rs, ws)) i effs
    in i1

explicateEffect (rs, ws) i eff =
  let (ef, rc) =
        case break (\c -> c == ':') eff of
         (ef', []) -> (ef', "Unknown")
         (ef', _:rc') -> (ef', rc')
      i1 = if ef `elem` rs then addEffectOperand (ef ++ "_use") "use" rc i  else i
      i2 = if ef `elem` ws then addEffectOperand (ef ++ "_def") "def" rc i1 else i1
      i3 = if ef `elem` rs then addToOperands (ef ++ "_use") "uses" i2 else i2
      i4 = if ef `elem` ws then addToOperands (ef ++ "_def") "defines" i3 else i3
  in i4

addEffectOperand name typ rc i =
    let ps = [YString "register", YString typ, YString rc]
    in yApplyTo (YString "operands")
       (yAddToSeq (YMap [(YString name, YSeq ps)])) i

addToOperands name list i = yApplyTo (YString list) (yAddToSeq (YString name)) i

update regClasses i = foldl updateRegClass i regClasses

updateRegClass i regClass =
  let (operand,
       rclass) = second tail $ break (\c -> c == ':') regClass
      (us, ds) = iUseDefs i
  in doUpdateRegClass (operand `elem` us, operand `elem` ds) (operand, rclass) i

doUpdateRegClass (False, False) _ i = i
doUpdateRegClass ud (operand, rclass) i =
  let usedef = case ud of
        (True, True) -> "usedef"
        (True, False) -> "use"
        (False, True) -> "def"
      i' = updateOperandInInstr operand
           (YSeq [YString "register",YString usedef,YString rclass]) i
  in i'

updateOperandInInstr k v (YMap fs) =
  let fs' = map (updateOperandInFields k v) fs
  in (YMap fs')

updateOperandInFields k v (YString "operands", YSeq ops) =
  (YString "operands", YSeq (map (updateOperand k v) ops))
updateOperandInFields _ _ f = f

updateOperand k v op @ (YMap [(YString k', _)])
  | k == k' = (YMap [(YString k, v)])
  | otherwise = op

makeBound regClasses i = foldl makeRegClassBound i regClasses

makeRegClassBound i regClass =
  let operands = map (mkRegClassBound regClass) $ iOperands i
  in replaceField i (YString "operands", YSeq operands)

mkRegClassBound regClass (YMap [(k, YSeq [YString "register", _, YString rc])])
  | regClass == rc = YMap [(k, YString "bound")]

mkRegClassBound _ operand = operand

yApplyTo p f (YMap s) = YMap [(k, if k == p then f v else v) | (k, v) <- s]

yAddToSeq e (YSeq s) = YSeq (s ++ [e])
yAddToSeq e YNil = YSeq [e]

sideEffects affs = [e | (YOtherSideEffect e) <- toAffectsList affs]

dummySrcLoc = SrcLoc "" 1 1
name = UnQual . HsIdent

infoToIds iF is =
    let infoId  = [(iF i, [oId i]) | i <- is]
        info2id = sort $ M.toList $ M.fromListWith (++) infoId
    in info2id

opcOpsFunBind n ((us, ds), rhss) = funBind [opcPVar, HsPTuple [us, ds]] n rhss

hsModule n e i d = HsModule dummySrcLoc (Module n) e i d

hsExportDataType n = HsEThingAll (name n)

hsExportVar n = HsEVar (name n)

unisonImport = hsImport "Unison"

instructionDeclImport targetName =
    hsImport ("Unison.Target." ++ targetName ++ ".SpecsGen." ++
              targetName ++ "InstructionDecl")

registerDeclImport targetName =
    hsImport ("Unison.Target." ++ targetName ++ "." ++
              targetName ++ "RegisterDecl")

itineraryDeclImport targetName =
    hsImport ("Unison.Target." ++ targetName ++ ".SpecsGen." ++
              targetName ++ "ItineraryDecl")

registerClassDeclImport targetName =
    hsImport ("Unison.Target." ++ targetName ++ "." ++
              targetName ++ "RegisterClassDecl")

hsImport n = HsImportDecl dummySrcLoc (Module n) False Nothing Nothing

funBind as n = hsFunBind n as

targetInstructionCon targetName = targetName ++ "Instruction"

simpleOpcFunBind n = hsFunBind n [opcPVar]

simpleFun op n = hsFun n [op]

constantFun n rhs = hsFun n [] rhs

hsFunBind n h rhss = mkHsFun n h (HsGuardedRhss rhss)
hsFun n h rhs = mkHsFun n h (HsUnGuardedRhs rhs)
mkHsFun n h g =
    HsFunBind
    [
     HsMatch
     dummySrcLoc
     (HsIdent n)
     h
     g
     []
    ]

simpleErrorRhs n =
  simpleFun (toHsPVar "a") n
  (HsApp (toHsVar "error")
   (HsParen
    (HsInfixApp (toHsStr ("unmatched: " ++ n ++ " "))
     (HsQVarOp (UnQual $ HsSymbol "++"))
                (HsApp (toHsVar "show") (toHsVar "a")))))

hsDataDecl targetName cs =
  HsDataDecl dummySrcLoc [] (HsIdent (targetInstructionCon targetName)) []
  [HsConDecl dummySrcLoc (HsIdent c) [] | c <- cs]
  [name "Eq", name "Ord"]

hsItinDecl targetName cs =
  HsDataDecl dummySrcLoc [] (HsIdent (targetName ++ "Itinerary")) []
  [HsConDecl dummySrcLoc (HsIdent c) [] | c <- cs]
  [name "Eq", name "Read", name "Show"]

hsInstDecl targetName ss =
  HsInstDecl dummySrcLoc [] (name "Show")
  [HsTyVar (HsIdent (targetInstructionCon targetName))] ss

opcPVar = toHsPVar "i"

mkOpcRhs f iF (info, ids) =
    let sids   = sort ids
        idList = map f sids
        hsInfo = iF info
        i      = toHsVar "i"
        cond   = HsInfixApp i (HsQVarOp (name "elem")) (HsList idList)
    in HsGuardedRhs dummySrcLoc cond hsInfo

toHsStr = HsLit . HsString
toHsVar = HsVar . name
toHsInt = HsLit . HsInt
toHsCon = HsCon . name
toHsPStr = HsPLit . HsString
toHsPVar = HsPVar . HsIdent
toHsList = HsList

idToHsCon = toHsCon . toOpType

renameRedefs (l1, l2) = (l1, map (redefNameIfIn l1) l2)
redefNameIfIn l s = if s `elem` l then toRedefName s else s
toRedefName = (++ "'")

mkOperandMap a =
  let o2info  = map simplifyOperand a
      o2info' = M.fromList o2info
  in o2info'

toVarName "0"   = "zero"
toVarName (c:l) = toLower c : l

toVarList = HsPList . map toHsPVar

toOpType (c:l) = stringReplace "." "" (toUpper c : l)

opToHsVar = toHsVar . toVarName

toConstantExtendedOp op = op ++ "_ce"

isConstantExtendedOp op = "_ce" `isSuffixOf` op

stringReplace s1 s2 = T.unpack . T.replace (T.pack s1) (T.pack s2) . T.pack

moduleName targetName suffix =
    "Unison.Target." ++ targetName ++ ".SpecsGen." ++ suffix
