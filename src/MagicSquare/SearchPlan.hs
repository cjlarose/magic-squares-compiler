module MagicSquare.SearchPlan
  ( searchPlan
  )
  where

import Data.Matrix (Matrix, fromLists, rref, nrows, ncols, (!))
import Data.Ratio ((%), numerator, denominator)
import Data.Maybe (isNothing, fromJust)
import Data.List (find)

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

magicConstant :: Int -> Int
magicConstant n = n * (n ^ 2 + 1) `div` 2

augmentedMatrix :: Int -> Matrix Int
augmentedMatrix n = fromLists rows
  where
    sumsToMagicConstant :: [Int] -> [Int]
    sumsToMagicConstant xs = xs ++ [magicConstant n]

    rowConstraints :: [[Int]]
    rowConstraints = [ sumsToMagicConstant [ if i == row then 1 else 0 | i <- [0..n-1], j <- [0..n-1] ] | row <- [0..n-1] ]

    columnConstraints :: [[Int]]
    columnConstraints = [ sumsToMagicConstant [ if j == column then 1 else 0 | i <- [0..n-1], j <- [0..n-1] ] | column <- [0..n-1] ]

    mainDiagonalConstraint :: [Int]
    mainDiagonalConstraint = sumsToMagicConstant [ if i == j then 1 else 0 | i <- [0..n-1], j <- [0..n-1] ]

    skewDiagonalConstraint :: [Int]
    skewDiagonalConstraint = sumsToMagicConstant [ if i + j == n - 1 then 1 else 0 | i <- [0..n-1], j <- [0..n-1] ]

    rows :: [[Int]]
    rows = rowConstraints ++ columnConstraints ++ [mainDiagonalConstraint] ++ [skewDiagonalConstraint]

rrefConstraintMatrix :: Int -> Either String (Matrix Rational)
rrefConstraintMatrix n = rref rationalMatrix
  where
    rationalMatrix :: Matrix Rational
    rationalMatrix = (\x -> fromRational $ (fromIntegral x) % 1) <$> (augmentedMatrix n)

pivots :: (Eq a, Num a) => Matrix a -> [(Int, Int)]
pivots a = f [] [0..nrows a - 1]
  where
    f :: [(Int, Int)] -> [Int] -> [(Int, Int)]
    f acc [] = acc
    f acc (i : is)
      | isNothing pivot = acc
      | otherwise       = f (fromJust pivot : acc) is
      where
        pivot :: Maybe (Int, Int)
        pivot = find isNonZero indices

        isNonZero :: (Int, Int) -> Bool
        isNonZero (r, c) = a ! (r + 1, c + 1) /= 0

        indices :: [(Int, Int)]
        indices = [ (row, j) | row <- [i..nrows a-1], j <- [0..ncols a -1] ]

searchPlan :: Int -> [MatrixPosition]
searchPlan 4 = [ FreePosition (0, 0) -- a
               , FreePosition (3, 3) -- b
               , FreePosition (0, 3) -- c
               , InducedPosition (3, 0) [ ConstantTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (3, 3)
                                        , PositionWithCoefficientTerm (-1) (0, 3)
                                        ]
               , FreePosition (1, 1) -- d
               , InducedPosition (2, 2) [ ConstantTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (3, 3)
                                        , PositionWithCoefficientTerm (-1) (1, 1)
                                        ]
               , FreePosition (1, 2) -- e
               , InducedPosition (2, 1) [ PositionWithCoefficientTerm 1 (0, 0)
                                        , PositionWithCoefficientTerm 1 (3, 3)
                                        , PositionWithCoefficientTerm (-1) (1, 2)
                                        ]
               , FreePosition (0, 1) -- f
               , InducedPosition (0, 2) [ ConstantTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (0, 3)
                                        , PositionWithCoefficientTerm (-1) (0, 1)
                                        ]
               , InducedPosition (3, 1) [ ConstantTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (3, 3)
                                        , PositionWithCoefficientTerm (-1) (1, 1)
                                        , PositionWithCoefficientTerm 1 (1, 2)
                                        , PositionWithCoefficientTerm (-1) (0, 1)
                                        ]
               , InducedPosition (3, 2) [ PositionWithCoefficientTerm 2 (0, 0)
                                        , PositionWithCoefficientTerm 1 (3, 3)
                                        , PositionWithCoefficientTerm 1 (0, 3)
                                        , PositionWithCoefficientTerm 1 (1, 1)
                                        , PositionWithCoefficientTerm (-1) (1, 2)
                                        , PositionWithCoefficientTerm 1 (0, 1)
                                        , ConstantTerm (-34)
                                        ]
               , FreePosition (1, 0) -- g
               , InducedPosition (1, 3) [ ConstantTerm 34
                                        , PositionWithCoefficientTerm (-1) (1, 1)
                                        , PositionWithCoefficientTerm (-1) (1, 2)
                                        , PositionWithCoefficientTerm (-1) (1, 0)
                                        ]
               , InducedPosition (2, 0) [ PositionWithCoefficientTerm 1 (3, 3)
                                        , PositionWithCoefficientTerm 1 (0, 3)
                                        , PositionWithCoefficientTerm (-1) (1, 0)
                                        ]
               , InducedPosition (2, 3) [ PositionWithCoefficientTerm 1 (1, 1)
                                        , PositionWithCoefficientTerm 1 (1, 2)
                                        , PositionWithCoefficientTerm 1 (1, 0)
                                        , PositionWithCoefficientTerm (-1) (3, 3)
                                        , PositionWithCoefficientTerm (-1) (0, 3)
                                        ]
               ]
searchPlan _ = error "Unimplemented"
