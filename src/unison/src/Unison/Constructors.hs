{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se

Functions to construct the Unison program representation.

-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

Contributing authors:
  Daniel Lundén <daniel.lunden@sics.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Constructors
       (
        -- * Bundle constructors
        mkBundle,
        -- * SingleOperation constructors
        mkSingleOperation,
        mkPhi,
        mkKill,
        mkDefine,
        mkCombine,
        mkLow,
        mkHigh,
        mkSplit2,
        mkSplit4,
        mkIn,
        mkOut,
        mkEntry,
        mkReturn,
        mkExit,
        mkVirtualCopy,
        mkFun,
        mkCopy,
        mkLinear,
        mkBranch,
        mkCall,
        mkTailCall,
        mkFrameSetup,
        mkFrameDestroy,
        -- * Other constructors
        mkFunction,
        mkCompleteFunction,
        mkBlock,
        mkDummyBlock,
        mkAttributes,
        mkNullAttributes,
        mkBlockAttributes,
        mkNullBlockAttributes,
        mkFrameObject,
        mkJumpTableEntry,
        mkTemp,
        mkPreAssignedTemp,
        mkCompleteTemp,
        mkMOperand,
        mkNullTemp,
        mkOperandRef,
        mkBlockRef,
        mkRegister,
        mkBound,
        mkNullInstruction,
        mkVirtualInstruction,
        mkBarrierInstruction,
        mkTargetRegister,
        mkInfiniteRegister,
        mkUsage
       )
       where

import Unison.Base

mkBundle = Bundle

mkSingleOperation id inst =
    SingleOperation {
      oId  = id,
      oOpr = inst,
      oAs  = mkNullAttributes
    }

mkPhi id us d = mkSingleOperation id (Virtual Phi {oPhiUs = us, oPhiD = d})

mkKill id is ts = mkSingleOperation id
                  (Virtual Kill {oKillIs = is, oKillUs = ts})

mkDefine id ts = mkSingleOperation id (Virtual Define {oDefineDs = ts})

mkCombine id lu hu d = mkSingleOperation id
      (Virtual Combine {oCombineLowU = lu, oCombineHighU = hu, oCombineD = d})

mkLow id is u d = mkSingleOperation id (Virtual Low {oLowIs = is, oLowU = u, oLowD = d})

mkHigh id is u d = mkSingleOperation id (Virtual High {oHighIs = is, oHighU = u, oHighD = d})

mkSplit2 id u ld hd =
  mkSingleOperation id (Virtual Split2 {oSplit2U = u, oSplit2LowD = ld, oSplit2HighD = hd})

mkSplit4 id u lld lhd hld hhd =
  mkSingleOperation id (Virtual Split4 {oSplit4U = u, oSplit4LowLowD = lld, oSplit4LowHighD = lhd,
                                                      oSplit4HighLowD = hld, oSplit4HighHighD = hhd})

mkIn id ins = mkSingleOperation id (Virtual (Delimiter In {oIns = ins}))

mkOut id outs = mkSingleOperation id (Virtual (Delimiter Out {oOuts = outs}))

mkEntry id entry =
  mkSingleOperation id (Virtual (Delimiter Entry {oEntry = entry}))

mkReturn id ret =
  mkSingleOperation id (Virtual (Delimiter Return {oReturn = ret}))

mkExit id = mkSingleOperation id (Virtual (Delimiter Exit))

mkVirtualCopy id s d = mkSingleOperation id
                       (Virtual VirtualCopy {oVirtualCopyS = s,
                                             oVirtualCopyD = d})

mkFun id s d =
  mkSingleOperation id (Virtual Fun {oFunctionUs = s, oFunctionDs = d})

mkCopy id insts s us d ds =
    mkSingleOperation id Copy {oCopyIs = insts, oCopyS = s, oCopyUs = us,
                               oCopyD = d, oCopyDs = ds}

mkLinear id insts us ds =
    mkSingleOperation id (Natural Linear {oIs = insts, oUs = us, oDs = ds})

mkBranch id insts us =
    mkSingleOperation id (Natural Branch {oBranchIs = insts, oBranchUs = us})

mkCall id insts us =
    mkSingleOperation id (Natural Call {oCallIs = insts, oCallUs = us})

mkTailCall id insts us =
    mkSingleOperation id
    (Natural TailCall {oTailCallIs = insts, oTailCallUs = us})

mkFrameSetup id s = mkSingleOperation id (Virtual (Frame Setup {oSetupU = s}))

mkFrameDestroy id s =
    mkSingleOperation id (Virtual (Frame Destroy {oDestroyU = s}))

mkFunction = mkCompleteFunction [] ""

mkCompleteFunction comments name code cs rts ffobjs fobjs sp ss consts jt goal
  rfs src =
  Function {fComments = comments, fName = name, fCode = code, fCongruences = cs,
            fRematerializable = rts, fFixedStackFrame = ffobjs,
            fStackFrame = fobjs, fStackPointerOffset = sp, fStackArgSize = ss,
            fConstants = consts, fJumpTable = jt, fGoal = goal,
            fRemovedFreqs = rfs, fSource = src}

mkBlock = Block

mkDummyBlock = mkBlock (-1) mkNullBlockAttributes

mkAttributes reads writes call mem acts vcopy remat jtblocks btaken pres rorig =
    Attributes {aReads = reads, aWrites = writes, aCall = call,
                aMem = mem, aActivators = acts, aVirtualCopy = vcopy,
                aRemat = remat, aJTBlocks = jtblocks, aBranchTaken = btaken,
                aPrescheduled = pres, aRematOrigin = rorig}

mkNullAttributes = mkAttributes [] [] Nothing Nothing [] False False [] Nothing Nothing Nothing

mkBlockAttributes entry exit return freq split =
    BlockAttributes {aEntry = entry, aExit = exit, aReturn = return,
                     aFreq = freq, aSplit = split}

mkNullBlockAttributes = mkBlockAttributes False False False Nothing False

mkFrameObject = FrameObject

mkJumpTableEntry = JumpTableEntry

mkTemp t = Temporary (toInteger t) Nothing

mkPreAssignedTemp t r = Temporary (toInteger t) (Just r)

mkCompleteTemp t r = Temporary (toInteger t) r

mkMOperand id ts r = MOperand id ts r

mkNullTemp = NullTemporary

mkOperandRef = OperandRef . toInteger

mkBlockRef = BlockRef

mkRegister = Register

mkBound = Bound

mkNullInstruction = General NullInstruction

mkVirtualInstruction = General VirtualInstruction

mkBarrierInstruction = General BarrierInstruction

mkTargetRegister = TargetRegister

mkInfiniteRegister = InfiniteRegister

mkUsage r u o = Usage r u o 0
