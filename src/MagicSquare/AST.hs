module MagicSquare.AST
  ( ComputedResultTerm(..)
  , MatrixPosition(..)
  )
  where

data ComputedResultTerm = ConstantTerm Int | PositionWithCoefficientTerm Int (Int, Int)
data MatrixPosition = FreePosition (Int, Int)
                    | InducedPosition (Int, Int) [ComputedResultTerm]
