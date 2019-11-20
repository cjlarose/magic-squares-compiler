module MagicSquare.AST
  ( ComputedResultTerm(..)
  , MatrixPosition(..)
  )
  where

data ComputedResultTerm = ConstantIntegerTerm Int | PositionWithCoefficientTerm Int (Int, Int)
data MatrixPosition = FreePosition (Int, Int)
                    | InducedPosition (Int, Int) [ComputedResultTerm]
