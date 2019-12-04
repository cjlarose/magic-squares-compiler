module MagicSquare.SearchPlan.Serialize
  ( serialize
  )
  where

import Data.List (intercalate)
import Text.Printf (printf)
import Data.Ratio (numerator, denominator)

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

showConstant :: Rational -> String
showConstant q = printf "(%d/%d)" (numerator q) (denominator q)

showTerm :: ComputedResultTerm -> String
showTerm (ConstantTerm b) = showConstant b
showTerm (PositionWithCoefficientTerm a pos) = printf "%s%s" (showConstant a) (show pos)

showTerms :: [ComputedResultTerm] -> String
showTerms = intercalate " + " . map showTerm

serializePosition :: MatrixPosition -> String
serializePosition (FreePosition pos) = printf "free\t%s" $ show pos
serializePosition (InducedPosition pos terms) = printf "basic\t%s\t%s" (show pos) (showTerms terms)

serialize :: Int -> [MatrixPosition] -> String
serialize _ xs = (intercalate "\n" . map serializePosition $ xs) ++ "\n"
