{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( printModule
    ) where

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

defGlobalSquare :: Int -> Definition
defGlobalSquare n = GlobalDefinition globalVariableDefaults
  { name = Name "square"
  , LLVM.AST.Global.type' = AST.ArrayType
    { nArrayElements = fromIntegral n
    , elementType = AST.ArrayType
      { nArrayElements = fromIntegral n
      , elementType = AST.IntegerType 64
      }
    }
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
