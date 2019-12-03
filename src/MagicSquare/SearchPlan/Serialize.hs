module MagicSquare.SearchPlan.Serialize
  ( serialize
  )
  where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Data.List (intercalate, foldl')
import Text.Printf (printf)
import Data.Ratio (numerator, denominator)

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

tableToString :: [[String]] -> String
tableToString xs = (intercalate "\n" . map serializeRow $ [0..n-1]) ++ "\n"
  where
    n = length xs
    m = length . head $ xs

    textWidth :: Int -> Int
    textWidth j = maximum . map (\i -> length $ xs !! i !! j) $ [0..n-1]

    textWidths :: Map.Map Int Int
    textWidths = foldr (\j m -> Map.insert j (textWidth j) m) Map.empty [0..m-1]

    serializeRow :: Int -> String
    serializeRow i = intercalate " | " . map (\j -> printf ("%-" ++ show (textWidths ! j) ++ "s") (xs !! i !! j)) $ [0..m-1]

positionsToTable :: Int -> [MatrixPosition] -> [[String]]
positionsToTable n xs = map serializeRow [0..n-1]
  where
    freePositionsByCoord :: Map.Map (Int, Int) String
    freePositionsByCoord = snd $ foldl' f (0, Map.empty) xs
      where
        f :: (Int, Map.Map (Int, Int) String) -> MatrixPosition -> (Int, Map.Map (Int, Int) String)
        f (i,m) (FreePosition pos) = (i + 1, Map.insert pos (printf "x_%02d" i) m)
        f (i,m) (InducedPosition _ _) = (i, m)

    inducedPositionsByCoord :: Map.Map (Int, Int) String
    inducedPositionsByCoord = foldl' f Map.empty xs
      where
        showConstant :: Rational -> String
        showConstant q = printf "(%d/%d)" (numerator q) (denominator q)

        showTerm :: ComputedResultTerm -> String
        showTerm (ConstantTerm b) = showConstant b
        showTerm (PositionWithCoefficientTerm a pos) = printf "%s%s" (showConstant a) (freePositionsByCoord ! pos)

        showTerms :: [ComputedResultTerm] -> String
        showTerms = intercalate "+" . map showTerm

        f :: Map.Map (Int, Int) String -> MatrixPosition -> Map.Map (Int, Int) String
        f m (FreePosition pos) = m
        f m (InducedPosition pos terms) = Map.insert pos (showTerms terms) m

    indexByCoord :: Map.Map (Int, Int) String
    indexByCoord = Map.union freePositionsByCoord inducedPositionsByCoord

    serializePosition :: (Int, Int) -> String
    serializePosition pos = indexByCoord ! pos

    serializeRow :: Int -> [String]
    serializeRow i = map (curry serializePosition i) [0..n-1]

serialize :: Int -> [MatrixPosition] -> String
serialize n = tableToString . positionsToTable n
