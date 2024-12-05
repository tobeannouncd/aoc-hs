module Y2024.D01 (main) where

import AoC ( strip, count )
import Data.List (sort)
import Solution

main :: Solution m => m ()
main = do
  (xs,ys) <- unzip . map parse . lines . strip <$> getInput
  answer $ sum [abs (x-y) | (x,y) <- zip (sort xs) (sort ys)]
  answer $ sum [x * count x ys | x <- xs]


parse :: String -> (Int,Int)
parse s =
  case map read . words $ s of
    [x,y] -> (x,y)
    _ -> error "no parse"