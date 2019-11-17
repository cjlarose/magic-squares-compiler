{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( enumerationModule
    ) where

import Data.Char (ord)

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Type (i8, i32, ptr)
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Instruction as Instruction
import qualified LLVM.AST.CallingConvention as CallingConvention
import LLVM.AST.Global
import LLVM.AST.ParameterAttribute (ParameterAttribute(NoCapture, ReadOnly))

import Control.Monad.Except

matrixType :: Int -> Type
matrixType n = AST.ArrayType
  { nArrayElements = fromIntegral n
  , elementType = AST.ArrayType
    { nArrayElements = fromIntegral n
    , elementType = i32
    }
  }

defExternalPrintf :: Definition
defExternalPrintf = GlobalDefinition functionDefaults
  { name = Name "printf"
  , returnType = i32
  , parameters = ([Parameter (ptr i8) (Name "str") [NoCapture, ReadOnly]], True) -- TODO i8
  }

defGlobalSquare :: Int -> Definition
defGlobalSquare n = GlobalDefinition globalVariableDefaults
  { name = Name "square"
  , linkage = Linkage.Private
  , LLVM.AST.Global.type' = matrixType n
  , initializer = Just . Constant.AggregateZero . matrixType $ n
  }

formatString :: Int -> String
formatString _ = "%d\n"

formatStringType :: Int -> Type
formatStringType n = AST.ArrayType (fromIntegral . length . formatString $ n) i8

formatStringAddress :: Int -> Constant.Constant
formatStringAddress n =
  Constant.GetElementPtr
    True -- inbounds
    (Constant.GlobalReference (ptr . formatStringType $ n) (Name "format_str"))
    [ Constant.Int 64 0, Constant.Int 64 0 ]

defGlobalFormatStr :: Int -> Definition
defGlobalFormatStr n = GlobalDefinition globalVariableDefaults
  { name = Name "format_str"
  , linkage = Linkage.Private
  , isConstant = True
  , LLVM.AST.Global.type' = formatStringType n
  , initializer = Just $ Constant.Array i8 initialValue
  , LLVM.AST.Global.alignment = 1
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

defPrintSquare :: Int -> Definition
defPrintSquare n = GlobalDefinition functionDefaults
  { name = Name "print_square"
  , returnType = AST.VoidType
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "a00" :=
            Instruction.Load
              False -- volatile
              (ConstantOperand $ matrixElementAddress n 0 0)
              Nothing -- atomicity
              4 -- alignment
            []
        , Name "chars_printed" :=
            Instruction.Call
              (Just Instruction.Tail)
              CallingConvention.C -- calling convention
              [] -- return attributes
              (Right . ConstantOperand $
                Constant.GlobalReference
                  (ptr (FunctionType i32 [ptr i8] True))
                  (Name "printf"))
              [ (ConstantOperand (formatStringAddress n), [])
              , (LocalReference i32 (Name "a00"), [])
              ]
              [] -- function attributes
              [] -- instruction metadata
        ]
        (Do $ Ret Nothing [])

enumerationModule :: Int -> AST.Module
enumerationModule n = defaultModule
  { moduleName = "enumerate"
  , moduleDefinitions =
    [ defExternalPrintf
    , defGlobalSquare n
    , defGlobalFormatStr n
    , defPrintSquare n
    ]
  }
