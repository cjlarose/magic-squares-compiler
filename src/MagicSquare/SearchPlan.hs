module MagicSquare.SearchPlan
  ( searchPlan
  )
  where

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

searchPlan :: Int -> [MatrixPosition]
searchPlan 4 = [ FreePosition (0, 0) -- a
               , FreePosition (3, 3) -- b
               , FreePosition (0, 3) -- c
               , InducedPosition (3, 0) [ ConstantIntegerTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (3, 3)
                                        , PositionWithCoefficientTerm (-1) (0, 3)
                                        ]
               , FreePosition (1, 1) -- d
               , InducedPosition (2, 2) [ ConstantIntegerTerm 34
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
               , InducedPosition (0, 2) [ ConstantIntegerTerm 34
                                        , PositionWithCoefficientTerm (-1) (0, 0)
                                        , PositionWithCoefficientTerm (-1) (0, 3)
                                        , PositionWithCoefficientTerm (-1) (0, 1)
                                        ]
               , InducedPosition (3, 1) [ ConstantIntegerTerm 34
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
                                        , ConstantIntegerTerm (-34)
                                        ]
               , FreePosition (1, 0) -- g
               , InducedPosition (1, 3) [ ConstantIntegerTerm 34
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
