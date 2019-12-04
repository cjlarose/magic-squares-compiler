module MagicSquare.SearchPlan.Serialize
  ( serialize
  )
  where

import Data.List (intercalate)
import Text.Printf (printf)
import Data.Ratio (numerator, denominator)

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))
import MagicSquare.SearchPlan (planCost)

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
serialize n xs = unlines output
  where
    showCost :: String
    showCost = printf "# cost = %d" . planCost n $ xs

    showOrder :: String
    showOrder = printf "order\t%d" n

    output :: [String]
    output = showCost : showOrder : map serializePosition xs
