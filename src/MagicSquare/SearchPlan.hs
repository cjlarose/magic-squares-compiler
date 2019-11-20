module MagicSquare.SearchPlan
  ( searchPlan
  )
  where

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

searchPlan :: Int -> [MatrixPosition]
searchPlan 4 = [ FreePosition (0, 0)
               , FreePosition (3, 3)
               , FreePosition (0, 3)
               , InducedPosition (3, 0) [ ConstantIntegerTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (3, 3)
                                        , PositionWithCoefficientTerm (-1) (0, 3)
                                        ]
               ]
searchPlan _ = error "Unimplemented"
