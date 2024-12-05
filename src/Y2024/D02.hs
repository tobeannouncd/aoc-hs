module Y2024.D02 (main) where

import AoC (countIf, strip)
import Data.Ix (inRange)
import Data.List (inits, tails)
import Solution

main :: Solution m => m ()
main = do
  inp <- map (map read . words) . lines . strip <$> getInput
  answer $ countIf (safeBy pure) inp
  answer $ countIf (safeBy holes) inp

holes :: [a] -> [[a]]
holes xs = [i ++ t | (i, _ : t) <- zip (inits xs) (tails xs)]

safeBy :: ([Int] -> [[Int]]) -> [Int] -> Bool
safeBy f = any isSafe . f

isSafe :: [Int] -> Bool
isSafe = check . (zipWith (-) <*> drop 1)
 where
  check xs = any (\r -> all (inRange r) xs) [(-3, -1), (1, 3)]