module Y2024.D07 (main) where

import Solution
import AoC.Parsec hiding (getInput)
import Data.List.Lens (stripSuffix)


inputP :: Parser [(Integer, [Integer])]
inputP = sepEndBy1 p newline
 where
  p = (,)
    <$> int <* string ": "
    <*> sepBy1 int (char ' ')

main :: (Solution m) => m ()
main = do
  equations <- parse' inputP =<< getInput
  answer $ sum [t | (t,eqn) <- equations, check part1 t eqn]
  answer $ sum [t | (t,eqn) <- equations, check part2 t eqn]

check :: (Integer -> Integer -> [Integer]) -> Integer -> [Integer] -> Bool
check f x xs = 0 `elem` foldr (concatMap . f) [x] xs

part1,part2 :: Integer -> Integer -> [Integer]
part1 x y =
  [y-x | y >= x] ++
  [q | (q,0) <- [y `quotRem` x]]
part2 x y = part1 x y
  ++ [read p | Just p@(_:_) <- [stripSuffix (show x) (show y)]]
