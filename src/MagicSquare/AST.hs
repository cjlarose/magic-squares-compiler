module MagicSquare.AST
  ( ComputedResultTerm(..)
  , MatrixPosition(..)
  )
  where

data ComputedResultTerm = ConstantTerm Rational | PositionWithCoefficientTerm Rational (Int, Int)
data MatrixPosition = FreePosition (Int, Int)
                    | InducedPosition (Int, Int) [ComputedResultTerm]
