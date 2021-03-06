{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module MagicSquare.CodeGen.DefEnumerate (defEnumerate) where

import Data.Char (ord)
import Data.List (intercalate, foldl')
import qualified Data.ByteString.Short
import Data.ByteString.Short (ShortByteString)
import Control.Monad (foldM)
import Data.Ratio (numerator, denominator)

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
import LLVM.IRBuilder.Instruction (retVoid, add, phi, icmp, condBr, br, sub, shl, and, or, store, mul, load, call, sdiv)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Internal.SnocList (snoc)
import LLVM.AST.IntegerPredicate (IntegerPredicate(SLE, SGE, EQ))

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))
import MagicSquare.CodeGen.InstructionUtil (matrixElementAddress, emitNamedInstruction)

genDoWhile :: MonadIRBuilder m => m AST.Operand -> (Name -> Name -> m a) -> m ()
genDoWhile loopCondition loopBody = do
  loopEntry <- freshName "loop_entry"
  loopStart <- freshName "loop_start"
  loopBodyAfter <- freshName "loop_body_after"

  br loopEntry

  emitBlockStart loopEntry
  br loopStart

  emitBlockStart loopStart
  loopBody loopEntry loopBodyAfter
  br loopBodyAfter

  emitBlockStart loopBodyAfter
  loopEnd <- freshName "loop_end"
  res <- loopCondition
  condBr res loopStart loopEnd

  emitBlockStart loopEnd

genIterateOverStaticRange :: MonadIRBuilder m => Int -> Int -> (AST.Operand -> m a) -> m ()
genIterateOverStaticRange minVal maxVal loopBody = do
  nextValName <- freshName "next_val"

  let loopCondition = icmp SLE (AST.LocalReference i32 nextValName) (int32 . fromIntegral $ maxVal)
  genDoWhile loopCondition (\loopEntry loopBodyAfter -> do
    i <- phi [ (int32 . fromIntegral $ minVal, loopEntry)
             , (AST.LocalReference i32 nextValName, loopBodyAfter) ]
    loopBody i
    emitNamedInstruction nextValName i32 $ Instruction.Add False False i (int32 1) [])

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
  whenEndLabel <- freshName "when_end"

  condBr val whenTrueLabel whenEndLabel

  emitBlockStart whenTrueLabel
  ifSuccess
  br whenEndLabel

  emitBlockStart whenEndLabel
  return ()

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

evaluatePolynomial :: MonadIRBuilder m
                   => Int
                   -> [ComputedResultTerm]
                   -> m AST.Operand
evaluatePolynomial n formula = do
  entryOperands <- mapM (\term -> case term of
                                    ConstantTerm _ -> pure . int32 $ 1
                                    PositionWithCoefficientTerm _ (i, j) -> load (AST.ConstantOperand $ matrixElementAddress n i j) 4) formula
  let coefficients = map (\term -> case term of
                                     ConstantTerm k -> k
                                     PositionWithCoefficientTerm k _ -> k) formula
      divisor = foldl' lcm 1 . map denominator $ coefficients
      coefficientOperands = map (\q -> int32 $ (numerator q * divisor) `div` divisor) $ coefficients
  sumTerms <- sequence $ zipWith mul entryOperands coefficientOperands
  sum <- foldM add (int32 0) sumTerms
  quotient <- sdiv sum (int32 divisor)
  return sum

whenInBounds :: MonadIRBuilder m
             => Int
             -> AST.Operand
             -> m a
             -> m ()
whenInBounds n val ifSuccess = do
  gteLowerBound <- icmp SGE val (int32 1)
  genWhen gteLowerBound $ do
    lteUpperBound <- icmp SLE val (int32 . fromIntegral $ n * n)
    genWhen lteUpperBound ifSuccess
    return ()

ifValidComputedPosition :: MonadIRBuilder m
                        => Int
                        -> AST.Operand
                        -> MatrixPosition
                        -> (AST.Operand -> m a) -> m()
ifValidComputedPosition n taken (InducedPosition coord formula) ifSuccess = do
  val <- evaluatePolynomial n formula
  whenInBounds n val $ do
    ifNotYetTaken n taken val coord $ \newTaken -> do
      ifSuccess newTaken
      return ()

forEachPossibleValue :: MonadIRBuilder m
                     => Int
                     -> AST.Operand
                     -> MatrixPosition
                     -> (AST.Operand -> m a)
                     -> m ()
forEachPossibleValue n taken (InducedPosition coord formula) ifSuccess =
  ifValidComputedPosition n taken (InducedPosition coord formula) ifSuccess
forEachPossibleValue n taken (FreePosition coord) ifSuccess =
  forEachAvailableValue n taken coord ifSuccess

defEnumerate :: Int
             -> [MatrixPosition]
             -> AST.Definition
defEnumerate n positions = mdo
  AST.GlobalDefinition functionDefaults
    { name = Name "enumerate"
    , returnType = AST.VoidType
    , basicBlocks = blocks
    }
  where
    blocks :: [BasicBlock]
    blocks = execIRBuilder emptyIRBuilder blockBuilder

    printSquare :: AST.Operand
    printSquare = AST.ConstantOperand $
                    Constant.GlobalReference
                      (ptr (AST.FunctionType AST.VoidType [] False))
                      (Name "print_square")

    searchRoot :: MonadIRBuilder m => (AST.Operand -> m a) -> m ()
    searchRoot ifSuccess = ifSuccess (int32 0) >> return ()

    continueSearch :: MonadIRBuilder m
                   => ((AST.Operand -> m ()) -> m ())
                   -> MatrixPosition
                   -> ((AST.Operand -> m ()) -> m ())
    continueSearch acc position = \next -> acc (\taken -> forEachPossibleValue n taken position next)

    blockBuilder :: IRBuilder ()
    blockBuilder = do
      let callPrintSquare = call printSquare [] >> return ()
      let search = foldl' continueSearch searchRoot positions
      search $ const callPrintSquare
