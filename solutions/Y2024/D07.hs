module Y2024.D07 (main) where

import Solution
import AoC.Parsec hiding (getInput)
import Control.Monad (forM_)

inputP :: Parser [(Integer, [Integer])]
inputP = sepEndBy1 p newline
 where
  p = (,)
    <$> int <* string ": "
    <*> sepBy1 int (char ' ')

main :: (Solution m) => m ()
main = do
  equations <- parse' inputP =<< getInput
  forM_ [[(+),(*)], [(+),(*),concatInt]] $ \ops -> do
    answer $ sum [t | (t,eqn) <- equations, t `elem` combos ops eqn]

combos :: [a -> a -> a] -> [a] -> [a]
combos ops (x:xs) = foldl f [x] xs
 where
  f acc b = [a `op` b | a <- acc, op <- ops]
combos _ _ = undefined

concatInt :: Integer -> Integer -> Integer
concatInt a b = read $ show a ++ show b
