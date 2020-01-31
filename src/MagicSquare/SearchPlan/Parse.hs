module MagicSquare.SearchPlan.Parse
  ( parse
  )
  where

import Data.Maybe (catMaybes)

import Text.Parsec (ParseError, (<|>), many, many1, manyTill, try, eof, between, sepBy1)
import qualified Text.Parsec (parse)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (endOfLine, string, space, noneOf, digit, char, anyChar)
import Data.ByteString (ByteString)

import MagicSquare.AST (ComputedResultTerm(..), MatrixPosition(..))

nonnegativeInteger :: Parser Int
nonnegativeInteger = read <$> many1 digit

coordinatePair :: Parser (Int, Int)
coordinatePair = between (char '(') (char ')') $
  (\i j -> (i, j)) <$> (nonnegativeInteger <* char ',') <*> nonnegativeInteger

freePosition :: Parser MatrixPosition
freePosition = FreePosition <$> coordinatePair

term :: Parser ComputedResultTerm
term = undefined

basicPosition :: Parser MatrixPosition
basicPosition = InducedPosition <$> coordinatePair <*> terms
  where
    terms = sepBy1 term $ many space *> char '+' *> many space

positionDeclaration :: Parser MatrixPosition
positionDeclaration = (position "free" freePosition <|> position "basic" basicPosition) <* endOfLine
  where
    position :: String -> Parser MatrixPosition -> Parser MatrixPosition
    position posType p = string posType *> many1 space *> p

comment :: Parser String
comment = char '#' *> (manyTill anyChar $ try endOfLine)

positionOrComment :: Parser (Maybe MatrixPosition)
positionOrComment = (Nothing <$ comment) <|> (Just <$> positionDeclaration)

orderDeclaraton :: Parser Int
orderDeclaraton = string "order" *> many1 space *> (read <$> many1 digit) <* endOfLine

searchPlan :: Parser (Int, [MatrixPosition])
searchPlan = many comment *> ((\a b -> (a, b)) <$> orderDeclaraton <*> (catMaybes <$> (many1 positionOrComment <* eof)))

parse :: ByteString -> Either ParseError (Int, [MatrixPosition])
parse input = Text.Parsec.parse searchPlan "" input
