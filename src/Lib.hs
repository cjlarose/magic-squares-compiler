{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( printModule
    ) where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Type (i32)
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

matrixType :: Int -> Type
matrixType n = AST.ArrayType
  { nArrayElements = fromIntegral n
  , elementType = AST.ArrayType
    { nArrayElements = fromIntegral n
    , elementType = i32
    }
  }

defGlobalSquare :: Int -> Definition
defGlobalSquare n = GlobalDefinition globalVariableDefaults
  { name = Name "square"
  , linkage = Linkage.Private
  , LLVM.AST.Global.type' = matrixType n
  , initializer = Just . Constant.AggregateZero . matrixType $ n
  }

enumerationModule :: Int -> AST.Module
enumerationModule n = defaultModule
  { moduleName = "enumerate"
  , moduleDefinitions = [defGlobalSquare n]
  }

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

printModule :: Int -> IO ()
printModule n = toLLVM $ enumerationModule n
