module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Debug.Trace (traceShowId)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString (getContents)
import qualified LLVM.AST as AST
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import Text.Parsec (ParseError)

import MagicSquare.CodeGen.ModuleGen (enumerationModule)
import MagicSquare.SearchPlan (searchPlan)
import MagicSquare.SearchPlan.Serialize (serialize)
import MagicSquare.SearchPlan.Parse (parse)
import MagicSquare.AST (MatrixPosition)

toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

printModule :: Int -> [MatrixPosition] -> IO ()
printModule n = toLLVM . enumerationModule n

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

handleParseError :: ParseError -> IO ()
handleParseError err = do
  hPutStrLn stderr "Parse error:"
  hPutStrLn stderr . show $ err
  exitFailure

compile :: [String] -> IO ()
compile [] = do
  input <- Data.ByteString.getContents
  let parseResult = parse input
  either handleParseError (uncurry printModule) (traceShowId parseResult)
compile _ = do
  hPutStrLn stderr "usage: magic-squares-compiler-exe compile"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr $ "usage: magic-squares-compiler-exe <command> [<args>]"
      exitFailure
    ("plan":restArgs) -> plan restArgs
    ("compile":restArgs) -> compile restArgs
    (command:_) -> do
      hPutStrLn stderr $ "Unknown command " ++ command
      exitFailure
