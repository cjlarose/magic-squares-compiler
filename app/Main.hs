module Main where

import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Char8 as BS
import qualified LLVM.AST as AST
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

import MagicSquare.CodeGen.ModuleGen (enumerationModule)
import MagicSquare.SearchPlan (searchPlan)

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

printModule :: Int -> IO ()
printModule n = either (hPutStrLn stderr) (toLLVM . enumerationModule n) $ searchPlan n

main :: IO ()
main = printModule 4
