module AoC.ReadP (
  int,
  module Text.ParserCombinators.ReadP
) where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

int :: (Read a, Num a) => ReadP a
int = do
  sign <- option 1 ((-1) <$ char '-')
  val <- read <$> many1 (satisfy isDigit)
  return (sign * val)