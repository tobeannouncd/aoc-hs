module AoC.ReadP (
  int,
  nat,
  module Text.ParserCombinators.ReadP,
) where

import Data.Char (digitToInt, isDigit)
import Text.ParserCombinators.ReadP

int :: (Integral a) => ReadP a
int = (*) <$> option 1 (-1 <$ char '-') <*> nat

nat :: (Integral a) => ReadP a
nat = foldl f 0 <$> many1 (satisfy isDigit)
 where
  f a d = 10 * a + fromIntegral (digitToInt d)