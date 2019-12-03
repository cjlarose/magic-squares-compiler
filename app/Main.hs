module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Data.ByteString.Char8 as BS
import qualified LLVM.AST as AST
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

import MagicSquare.CodeGen.ModuleGen (enumerationModule)
import MagicSquare.SearchPlan (searchPlan)
import MagicSquare.SearchPlan.Serialize (serialize)

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

printModule :: Int -> IO ()
printModule n = either (hPutStrLn stderr) (toLLVM . enumerationModule n) $ searchPlan n

plan :: [String] -> IO ()
plan (arg0:_) = do
  let n = read arg0 :: Int
  let planResult = searchPlan n
  case planResult of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right plan -> do
      putStr . serialize n $ plan
plan _ = do
  hPutStrLn stderr "usage: magic-squares-compiler-exe plan n"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printModule 4
    ("plan":restArgs) -> plan restArgs
    (command:_) -> do
      hPutStrLn stderr $ "Unknown command " ++ command
      exitFailure
