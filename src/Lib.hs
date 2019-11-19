{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Lib
    ( enumerationModule
    ) where

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

matrixType :: Int -> AST.Type
matrixType n = AST.ArrayType (fromIntegral n) (AST.ArrayType (fromIntegral n) i32)

defExternalPrintf :: AST.Definition
defExternalPrintf = AST.GlobalDefinition functionDefaults
  { name = Name "printf"
  , returnType = i32
  , parameters = ([Parameter (ptr i8) (Name "str") [NoCapture, ReadOnly]], True)
  }

defGlobalSquare :: Int -> AST.Definition
defGlobalSquare n = AST.GlobalDefinition globalVariableDefaults
  { name = Name "square"
  , linkage = Linkage.Private
  , type' = matrixType n
  , initializer = Just . Constant.AggregateZero . matrixType $ n
  , alignment = 64
  }

formatString :: Int -> String
formatString n = (intercalate " " $ take (n * n) (repeat "%d")) ++ "\n"

formatStringType :: Int -> AST.Type
formatStringType n = AST.ArrayType (fromIntegral . length . formatString $ n) i8

formatStringAddress :: Int -> Constant.Constant
formatStringAddress n =
  Constant.GetElementPtr
    True -- inbounds
    (Constant.GlobalReference (ptr . formatStringType $ n) (Name "format_str"))
    [ Constant.Int 64 0, Constant.Int 64 0 ]

defGlobalFormatStr :: Int -> AST.Definition
defGlobalFormatStr n = AST.GlobalDefinition globalVariableDefaults
  { name = Name "format_str"
  , linkage = Linkage.Private
  , isConstant = True
  , type' = formatStringType n
  , initializer = Just $ Constant.Array i8 initialValue
  , alignment = 1
  }
  where
    str :: String
    str = formatString n

    initialValue :: [Constant.Constant]
    initialValue = map (\x -> Constant.Int 8 . fromIntegral $ ord x) str

matrixElementAddress :: Int -> Int -> Int -> Constant.Constant
matrixElementAddress n i j =
  Constant.GetElementPtr
    True -- inbounds
    (Constant.GlobalReference (ptr (matrixType n)) (Name "square"))
    [ Constant.Int 64 0
    , Constant.Int 64 . fromIntegral $ i
    , Constant.Int 64 . fromIntegral $ j
    ]

defPrintSquare :: Int -> AST.Definition
defPrintSquare n = AST.GlobalDefinition functionDefaults
  { name = Name "print_square"
  , returnType = AST.VoidType
  , basicBlocks = [body]
  }
  where
    matrixCoods :: [(Int, Int)]
    matrixCoods = [(i,j) | i <- [0..n-1], j <- [0..n-1]]

    matrixValueLocalName :: (Int, Int) -> Name
    matrixValueLocalName (i, j) = Name . Data.ByteString.Short.pack . map (fromIntegral . fromEnum) $ name
      where name = "a" ++ show i ++ "_" ++ show j

    loadInstruction :: (Int, Int) -> Instruction.Named Instruction.Instruction
    loadInstruction (i, j) =
      matrixValueLocalName (i, j) :=
          Instruction.Load
            False -- volatile
            (AST.ConstantOperand $ matrixElementAddress n i j)
            Nothing -- atomicity
            4 -- alignment
            [] -- instruction metadata

    loadInstructions :: [Instruction.Named Instruction.Instruction]
    loadInstructions = map loadInstruction matrixCoods

    body = BasicBlock
        (Name "entry")
        ( loadInstructions ++
          [ Name "chars_printed" :=
              Instruction.Call
                (Just Instruction.Tail)
                CallingConvention.C -- calling convention
                [] -- return attributes
                (Right . AST.ConstantOperand $
                  Constant.GlobalReference
                    (ptr (AST.FunctionType i32 [ptr i8] True))
                    (Name "printf"))
                ( (AST.ConstantOperand (formatStringAddress n), []) :
                  (map (\x -> (AST.LocalReference i32 (matrixValueLocalName x), [])) matrixCoods)
                )
                [] -- function attributes
                [] -- instruction metadata
          ]
        )
        (Instruction.Do $ Instruction.Ret Nothing [])

toShortByteString :: String -> ShortByteString
toShortByteString = Data.ByteString.Short.pack . map (fromIntegral . fromEnum)

emitNamedInstruction :: MonadIRBuilder m => Name -> AST.Type -> Instruction.Instruction -> m AST.Operand
emitNamedInstruction name returnType instr = do
  modifyBlock $ \bb -> bb { partialBlockInstrs = partialBlockInstrs bb `snoc` (name := instr) }
  pure $ AST.LocalReference returnType name

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

defMain :: AST.Definition
defMain = AST.GlobalDefinition functionDefaults
  { name = Name "main"
  , returnType = i32
  , basicBlocks = [body]
  }
  where
    body :: BasicBlock
    body = BasicBlock
      (Name "entry")
      [ Instruction.Do $
          Instruction.Call
          Nothing
          CallingConvention.C
          [] -- return attributes
          ( Right . AST.ConstantOperand $
              Constant.GlobalReference
                (ptr (AST.FunctionType AST.VoidType [] False))
                (Name "print_square") )
          [] -- arguments
          [] -- function attributes
          [] -- instruction attributes
      ]
      (Instruction.Do $ Instruction.Ret (Just (AST.ConstantOperand $ Constant.Int 32 0)) [])

enumerationModule :: Int -> AST.Module
enumerationModule n = AST.defaultModule
  { AST.moduleName = "enumerate"
  , AST.moduleDefinitions =
    [ defGlobalSquare n
    , defGlobalFormatStr n
    , defExternalPrintf
    , defPrintSquare n
    , defEnumerate n []
    , defMain
    ]
  }
