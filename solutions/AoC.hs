module AoC (
  strip,
  countIf,
  count,
  eitherToFail,
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Data (cast, Typeable)

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

count :: (Eq a) => a -> [a] -> Int
count = countIf . (==)

eitherToFail :: (MonadFail m, Typeable a, Show a) => Either a b -> m b
eitherToFail = either (fail . show') return
 where
  show' x
    | Just s <- cast x = fail s
    | otherwise        = fail $ show x