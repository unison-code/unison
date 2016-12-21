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
module Unison.Transformations.FoldReservedRegisters
       (foldReservedRegisters) where

import Unison.Base
import Unison.Util
import Unison.Predicates
import Unison.Target.API

foldReservedRegisters f target =
  let rf = reserved target
      f' = peephole (cleanResRegCopies rf) $
           peephole (foldResRegAssignment rf) f
  in f'


foldResRegAssignment rf f (o : rest) _ =
  let fcode = flatCode f
      o'    = mapToOperandIf (isReservedTemp rf fcode) (foldTempReg fcode) o
  in (rest, [o'])

isReservedTemp rf fcode t @ Temporary {} =
  case definer t fcode of
    SingleOperation {
      oOpr = Virtual (VirtualCopy {oVirtualCopyS = r @ Register {}})}
      | isReservedReg rf r -> True
    _ ->  False
isReservedTemp _ _ _ = False

isReservedReg rf r = rTargetReg (regId r) `elem` rf

foldTempReg fcode t = oSingleUse $ definer t fcode

cleanResRegCopies rf _ (
  SingleOperation {oOpr = Virtual (VirtualCopy {oVirtualCopyS = r,
                                                oVirtualCopyD = r'})}
  :
  rest) _ | isRegister r && isRegister r' && r == r' && isReservedReg rf r =
  (rest, [])

cleanResRegCopies _ _ (o : rest) _ = (rest, [o])
