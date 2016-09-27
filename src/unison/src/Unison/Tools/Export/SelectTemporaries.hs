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
module Unison.Tools.Export.SelectTemporaries (selectTemporaries) where

import Data.List
import qualified Data.Map as M

import Unison

selectTemporaries temporaries f @ Function {fCode = code} _target =
    let t2r   = M.fromList $ [(mkTemp t, r) | (Temporary t (Just r))
                                                <- uniqueTemps (flatten code)]
        p2t   = M.fromList $ zip (map mkOperandRef [0..]) temporaries
        code' = mapToOperationInBlocks (selectTemporariesInInstr p2t t2r) code
    in f {fCode = code', fCongruences = []}

selectTemporariesInInstr p2t t2r i =
    mapToOperandIf isMOperand (pickChoice p2t t2r) i

pickChoice :: Ord r =>
              M.Map (Operand r) TemporaryId ->
              M.Map (Operand r) (Operand r) ->
              Operand r -> Operand r
pickChoice p2t t2r MOperand {operandId = p} =
    case p2t M.! mkOperandRef p of
      (-1) -> mkNullTemp
      t    ->  t2r M.! (mkTemp t)

-- Note: we cannot use tUniqueOps from Unison.Util since it propagates
-- operand preassignments to temporaries
uniqueTemps :: Eq r => [BlockOperation i r] -> [Operand r]
uniqueTemps = nub . concatMap oTemps
oTemps = concatMap extractOriginalTemps . oAllOps
extractOriginalTemps MOperand {altTemps = ts} =
                     filter (not . isNullTemporary) ts
extractOriginalTemps _ = []
