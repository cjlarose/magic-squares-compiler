module MagicSquare.SearchPlan
  ( searchPlan
  )
  where

import Data.Matrix (Matrix, fromLists, rref, nrows, ncols, (!))
import Data.Ratio ((%), numerator, denominator)
import Data.Maybe (isNothing, fromJust, listToMaybe)
import Data.List (find, foldl')
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

positions :: Int -> Matrix Rational -> [MatrixPosition]
positions n a = map positionAtCol [0..ncols a - 2]
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


matrixPositions :: Int -> Either String [MatrixPosition]
matrixPositions n = positions n <$> reduced
  where
    reduced :: Either String (Matrix Rational)
    reduced = rref . rationalMatrix . constraintMatrix $ n

data Vertex = SearchStart | Choice MatrixPosition | SearchEnd deriving (Eq, Ord, Show)

topoSort :: [MatrixPosition] -> [MatrixPosition]
topoSort xs = search . Set.singleton $ (0, [SearchStart])
  where
    choices :: [Vertex] -> [MatrixPosition]
    choices = reverse . foldl' (\acc x -> case x of
                                  Choice y -> y : acc
                                  _        -> acc) []

    isInduced :: Vertex -> Bool
    isInduced (Choice (InducedPosition _ _)) = True
    isInduced _                              = False

    vertices :: Set.Set Vertex
    vertices = Set.fromList $ SearchStart : SearchEnd : map Choice xs

    dependencies :: Vertex -> Set.Set Vertex
    dependencies SearchStart = Set.empty
    dependencies SearchEnd = Set.fromList . map Choice $ xs
    dependencies (Choice (FreePosition _)) = Set.singleton SearchStart
    dependencies (Choice (InducedPosition _ terms)) =
      foldl' (\acc term -> case term of
                             PositionWithCoefficientTerm _ p -> Set.insert (Choice (FreePosition p)) acc
                             _ -> acc) Set.empty terms

    search :: Set.Set (Int, [Vertex]) -> [MatrixPosition]
    search q = case minV of
                 SearchEnd -> reverse . choices $ minPath
                 _ -> search newQueue
      where
        withoutMin :: Set.Set (Int, [Vertex])
        ((minDistance, minPath), withoutMin) = Set.deleteFindMin q
        (minV : _) = minPath

        computedPositions :: Set.Set Vertex
        computedPositions = Set.fromList minPath

        computedInducedPositions :: Set.Set Vertex
        computedInducedPositions = Set.filter isInduced computedPositions

        remainingPositions :: Set.Set Vertex
        remainingPositions = Set.difference vertices computedPositions

        satisfiable :: Vertex -> Bool
        satisfiable x = Set.isSubsetOf (dependencies x) computedPositions

        satisfiableDependents :: Set.Set Vertex
        satisfiableDependents = Set.filter satisfiable remainingPositions

        updateQueue :: Set.Set (Int, [Vertex]) -> Vertex -> Set.Set (Int, [Vertex])
        updateQueue q v =
          let newPath = v : minPath
              newCost = case v of
                          SearchEnd -> minDistance
                          Choice (FreePosition _) -> (max 1 minDistance) * (length xs - (Set.size computedInducedPositions))
                          Choice (InducedPosition _ _) -> minDistance
              existingEl = listToMaybe . Set.toList . Set.filter (\(_, (u : _)) -> u == v) $ q
              newEl = (newCost, newPath)
          in Set.insert newEl q

        newQueue :: Set.Set (Int, [Vertex])
        newQueue = foldl' updateQueue withoutMin satisfiableDependents

searchPlan :: Int -> Either String [MatrixPosition]
searchPlan n = topoSort <$> matrixPositions n
