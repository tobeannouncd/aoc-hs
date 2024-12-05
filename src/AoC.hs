module AoC (
  strip,
  countIf,
  count,
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

count :: (Eq a) => a -> [a] -> Int
count = countIf . (==)