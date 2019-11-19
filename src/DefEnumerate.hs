{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module DefEnumerate (defEnumerate) where

import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.ByteString.Short
import Data.ByteString.Short (ShortByteString)

import qualified LLVM.AST as AST
import LLVM.AST (Name(Name), Named((:=)))
import LLVM.AST.Type (i8, i32, ptr)
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Instruction as Instruction
import qualified LLVM.AST.CallingConvention as CallingConvention
import LLVM.AST.Global
  ( name
  , returnType
  , parameters
  , linkage
  , initializer
  , isConstant
  , basicBlocks
  , type'
  , alignment
  , globalVariableDefaults
  , functionDefaults
  , BasicBlock(..)
  , Parameter(..)
  )
import LLVM.AST.ParameterAttribute (ParameterAttribute(NoCapture, ReadOnly))
import LLVM.IRBuilder.Monad
  ( block
  , named
  , freshName
  , partialBlockInstrs
  , modifyBlock
  , emitBlockStart
  , emitTerm
  , execIRBuilder
  , emptyIRBuilder
  , IRBuilder
  , MonadIRBuilder
  )
import LLVM.IRBuilder.Instruction (retVoid, add, phi, icmp, condBr, br, sub, shl, and, or, store)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Internal.SnocList (snoc)
import LLVM.AST.IntegerPredicate (IntegerPredicate(SLE, EQ))

import InstructionUtil (matrixElementAddress, emitNamedInstruction)

genIterateOverStaticRange :: MonadIRBuilder m => Int -> Int -> (AST.Operand -> m a) -> m Name
genIterateOverStaticRange minVal maxVal loopBody = do
  entry <- block `named` "loop_entry"
  loopStart <- freshName "loop_start"
  loopBodyAfter <- freshName "loop_body_after"
  br loopStart

  emitBlockStart loopStart
  nextValName <- freshName "next_val"
  i <- phi [ (int32 . fromIntegral $ minVal, entry)
           , (AST.LocalReference i32 nextValName, loopBodyAfter) ]
  loopBody i
  br loopBodyAfter

  emitBlockStart loopBodyAfter
  emitNamedInstruction nextValName i32 $ Instruction.Add False False i (int32 1) []
  res <- icmp SLE i (int32 . fromIntegral $ maxVal)
  loopEnd <- freshName "loop_end"
  condBr res loopStart loopEnd

  emitBlockStart loopEnd
  return entry

isFree :: MonadIRBuilder m => AST.Operand -> AST.Operand -> m AST.Operand
isFree taken val = do
  lessOne <- sub val (int32 1)
  shifted <- shl (int32 1) lessOne
  val <- LLVM.IRBuilder.Instruction.and taken shifted
  icmp LLVM.AST.IntegerPredicate.EQ val (int32 0)

setTaken :: MonadIRBuilder m => AST.Operand -> AST.Operand -> m AST.Operand
setTaken taken val = do
  lessOne <- sub val (int32 1)
  shifted <- shl (int32 1) lessOne
  LLVM.IRBuilder.Instruction.or taken shifted

genWhen :: MonadIRBuilder m => AST.Operand -> m a -> m ()
genWhen val ifSuccess = mdo
  whenTrueLabel <- freshName "when_true"
  whenFalseLabel <- freshName "when_false"

  condBr val whenTrueLabel whenFalseLabel
  emitBlockStart whenTrueLabel
  ifSuccess
  emitBlockStart whenFalseLabel

setMatrixValue :: MonadIRBuilder m => Int -> (Int, Int) -> AST.Operand -> m ()
setMatrixValue n (i, j) val = do
  let addr = AST.ConstantOperand $ matrixElementAddress n i j
  store addr 4 val

ifNotYetTaken :: MonadIRBuilder m => Int -> AST.Operand -> AST.Operand -> (Int, Int) -> (AST.Operand -> m a) -> m ()
ifNotYetTaken n taken val coord ifSuccess = mdo
  free <- isFree taken val
  genWhen free $ mdo
    newTaken <- setTaken taken val
    setMatrixValue n coord val
    ifSuccess newTaken

forEachAvailableValue :: MonadIRBuilder m => Int -> AST.Operand -> (Int, Int) -> (AST.Operand -> m a) -> m()
forEachAvailableValue n taken coord ifAvailable = mdo
  genIterateOverStaticRange 1 (n * n) $ \param -> mdo
    ifNotYetTaken n taken param coord $ \newTaken -> mdo
      ifAvailable newTaken
      return ()
    return ()
  return ()

data ComputedResultTerm = ConstantInteger Int | PositionWithCoefficient Int (Int, Int)
data MatrixPosition = FreePosition (Int, Int)
                    | InducedPosition (Int, Int) [ComputedResultTerm]

defEnumerate :: Int
             -> [MatrixPosition]
             -> AST.Definition
defEnumerate n [] = mdo
  AST.GlobalDefinition functionDefaults
    { name = Name "enumerate"
    , returnType = AST.VoidType
    , basicBlocks = blocks
    }
  where
    blocks :: [BasicBlock]
    blocks = execIRBuilder emptyIRBuilder blockBuilder

    blockBuilder :: IRBuilder ()
    blockBuilder = forEachAvailableValue n (int32 0) (0, 0) $ \g -> return ()
