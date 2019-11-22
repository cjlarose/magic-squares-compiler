module MagicSquare.SearchPlan
  ( searchPlan
  , rrefConstraintMatrix
  , pivots
  )
  where

import Data.Matrix (Matrix, fromLists, rref, nrows, ncols, (!))
import Data.Ratio ((%), numerator, denominator)
import Data.Maybe (isNothing, fromJust)
import Data.List (find, maximumBy, partition, foldl')
import Data.Ord (comparing)
import qualified Data.Set as Set

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

magicConstant :: Int -> Int
magicConstant n = n * (n ^ 2 + 1) `div` 2

constraintMatrix :: Int -> Matrix Int
constraintMatrix n = fromLists rows
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

rationalMatrix :: Matrix Int -> Matrix Rational
rationalMatrix = fmap (\x -> fromRational $ (fromIntegral x) % 1)

pivots :: (Eq a, Num a) => Matrix a -> [(Int, Int)]
pivots a = f [] [0..nrows a - 1]
  where
    f :: [(Int, Int)] -> [Int] -> [(Int, Int)]
    f acc [] = acc
    f acc (i : is)
      | isNothing pivot = reverse acc
      | otherwise       = f (fromJust pivot : acc) is
      where
        pivot :: Maybe (Int, Int)
        pivot = find isNonZero indices

        isNonZero :: (Int, Int) -> Bool
        isNonZero (r, c) = a ! (r + 1, c + 1) /= 0

        indices :: [(Int, Int)]
        indices = [ (row, j) | row <- [i..nrows a-1], j <- [0..ncols a -1] ]

matrixPositions :: Int -> Either String [MatrixPosition]
matrixPositions n = positions <$> reduced
  where
    reduced :: Either String (Matrix Rational)
    reduced = rref . rationalMatrix . constraintMatrix $ n

    positions :: Matrix Rational -> [MatrixPosition]
    positions a = map positionAtCol [0..ncols a - 2]
      where
        ps :: [(Int, Int)]
        ps = pivots a

        findPivot :: Int -> Maybe (Int, Int)
        findPivot j = find (\(_, jj) -> j == jj) ps

        originalPosition :: Int -> (Int, Int)
        originalPosition j = (j `div` n, j `mod` n)

        entryAt :: (Int, Int) -> Rational
        entryAt (i, j) = a ! (i + 1, j + 1)

        termForPosition :: (Int, Int) -> ComputedResultTerm
        termForPosition (i, j) | j == ncols a - 1 = ConstantTerm . entryAt $ (i, j)
                               | otherwise        = PositionWithCoefficientTerm (- entryAt (i, j)) $ originalPosition j

        equationForPivot :: (Int, Int) -> [ComputedResultTerm]
        equationForPivot (i, j) = map termForPosition . filter (\p -> entryAt p /= 0) $ [ (i, jj) | jj <- [j + 1..ncols a - 1] ]

        positionAtCol :: Int -> MatrixPosition
        positionAtCol j = case findPivot j of
                            Just p  -> InducedPosition (originalPosition j) $ equationForPivot p
                            Nothing -> FreePosition . originalPosition $ j

topoSort :: [MatrixPosition] -> [MatrixPosition]
topoSort xs = f [] xs
  where
    f :: [MatrixPosition] -> [MatrixPosition] -> [MatrixPosition]
    f acc remaining
      | null remaining = reverse acc
      | otherwise      = f (bestChoice : acc) leftover
      where
        isFree :: MatrixPosition -> Bool
        isFree (FreePosition _) = True
        isFree _ = False

        remainingFreePositions :: [MatrixPosition]
        remainingInducedPositions :: [MatrixPosition]
        (remainingFreePositions, remainingInducedPositions) = partition isFree remaining

        computedFreePositions :: Set.Set (Int, Int)
        computedFreePositions = Set.fromList . map coord . filter isFree $ acc

        dependencies :: MatrixPosition -> Set.Set (Int, Int)
        dependencies (InducedPosition _ terms) =
          foldl' (\acc term -> case term of
                                 PositionWithCoefficientTerm _ p -> Set.insert p acc
                                 _ -> acc) Set.empty terms

        satisfiedBy :: Set.Set (Int, Int) -> MatrixPosition -> Bool
        satisfiedBy computed inducedPosition = Set.isSubsetOf (dependencies inducedPosition) computed

        satisfiableDependents :: MatrixPosition -> [MatrixPosition]
        satisfiableDependents (FreePosition p) = filter (satisfiedBy (Set.insert p computedFreePositions)) remainingInducedPositions

        choices :: [(MatrixPosition, [MatrixPosition])]
        choices = map (\p -> (p, satisfiableDependents p)) remainingFreePositions

        bestChoice :: MatrixPosition
        bestChoice = case find (satisfiedBy computedFreePositions) remainingInducedPositions of
                       Just p  -> p
                       Nothing -> fst $ maximumBy (comparing $ length . snd) choices

        coord :: MatrixPosition -> (Int, Int)
        coord (FreePosition c) = c
        coord (InducedPosition c _) = c

        leftover :: [MatrixPosition]
        leftover = filter (\mp -> coord mp /= coord bestChoice) remaining

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
