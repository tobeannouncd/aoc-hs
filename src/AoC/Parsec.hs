module AoC.Parsec (
  module Text.Parsec,
  module Text.Parsec.String,
  nat, int, parse',
) where

import Text.Parsec
import Text.Parsec.String
import Data.Char (digitToInt)

nat :: Integral a => Parser a
nat = toNat <$> many1 digit
  where
    toNat = foldl1 (\a d -> 10*a + d) . map (fromIntegral . digitToInt)

int :: Integral a => Parser a
int = (*)
  <$> option 1 ((-1) <$ char '-')
  <*> nat

parse' :: (MonadFail m) => Parser a -> String -> m a
parse' p = either (fail . show) return . parse p ""