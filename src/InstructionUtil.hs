{-# LANGUAGE OverloadedStrings #-}

module InstructionUtil
  ( matrixType
  , matrixElementAddress
  , emitNamedInstruction
  ) where

import qualified LLVM.AST as AST
import LLVM.AST (Name(Name), Named((:=)))
import LLVM.AST.Type (i32, ptr)
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Instruction as Instruction
import LLVM.IRBuilder.Monad
  ( partialBlockInstrs
  , modifyBlock
  , IRBuilder
  , MonadIRBuilder
  )
import LLVM.IRBuilder.Internal.SnocList (snoc)

matrixType :: Int -> AST.Type
matrixType n = AST.ArrayType (fromIntegral n) (AST.ArrayType (fromIntegral n) i32)

matrixElementAddress :: Int -> Int -> Int -> Constant.Constant
matrixElementAddress n i j =
  Constant.GetElementPtr
    True -- inbounds
    (Constant.GlobalReference (ptr (matrixType n)) (Name "square"))
    [ Constant.Int 64 0
    , Constant.Int 64 . fromIntegral $ i
    , Constant.Int 64 . fromIntegral $ j
    ]

emitNamedInstruction :: MonadIRBuilder m => Name -> AST.Type -> Instruction.Instruction -> m AST.Operand
emitNamedInstruction name returnType instr = do
  modifyBlock $ \bb -> bb { partialBlockInstrs = partialBlockInstrs bb `snoc` (name := instr) }
  pure $ AST.LocalReference returnType name

