module Y2024.D08 (main) where

import Solution
import AoC.Coord (from2dString)
import qualified Data.Map.Strict as Map
import Data.Ix
import qualified Data.Set as Set

main :: (Solution m) => m ()
main = do
  input <- from2dString <$> getInput
  let byFreq = Map.fromListWith (++) [(v,[k]) | (k,v) <- input, v /= '.']
      inGrid = inRange (fst $ head input, fst $ last input)
  answer $ solveWith (part1 inGrid) byFreq
  answer $ solveWith (part2 inGrid) byFreq

solveWith :: Ord a1 => (a2 -> [a1]) -> Map.Map k a2 -> Int
solveWith f = Set.size . Set.unions . map (Set.fromList . f) . Map.elems

part1,part2 :: Num a => (a -> Bool) -> [a] -> [a]
part1 inGrid pts =
  [ pt
  | (a,b) <- pairs pts
  , pt <- [2*a-b, 2*b-a]
  , inGrid pt ]
part2 inGrid pts =
  [ pt
  | (a,b) <- pairs pts
  , (start, dir) <- [(a,a-b),(b,b-a)]
  , pt <- takeWhile inGrid $ iterate (+ dir) start ]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs