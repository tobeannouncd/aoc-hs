module Y2024.D10 (main) where

import AoC (countIf)
import AoC.Coord
import AoC.Search (dfs)
import Data.Char
import Data.Map qualified as Map
import Solution

main :: (Solution m) => m ()
main = do
  input <- getInput
  let heights = Map.fromList [(c, digitToInt v) | (c, v) <- from2dString input]
      starts = [c | (c, 0) <- Map.toList heights]
  answer $ sum . map (part1 heights) $ starts
  answer $ sum . map (part2 heights) $ starts

part1, part2 :: Map.Map Coord Int -> Coord -> Int
part1 heights = countIf isEnd . dfs next
 where
  next here =
    [ there
    | there <- cardinal here
    , Just h <- [heights Map.!? there]
    , h == 1 + heights Map.! here
    ]
  isEnd pt = heights Map.! pt == 9
part2 heights = pathsFrom
 where
  pathsFrom here =
    let h = heights Map.! here
     in if h == 9
          then 1
          else
            sum
              [ pathsFrom there
              | there <- cardinal here
              , Just h' <- [heights Map.!? there]
              , h + 1 == h'
              ]