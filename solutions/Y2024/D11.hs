module Y2024.D11 (main) where

import Data.IntMap qualified as Map
import Solution

main :: (Solution m) => m ()
main = do
  stones <- Map.fromListWith (+) . map ((,1) . read) . words <$> getInput
  let blinks = map sum $ iterate blink stones
  answer $ blinks !! 25
  answer $ blinks !! 75

blink :: Map.IntMap Int -> Map.IntMap Int
blink = Map.foldrWithKey f Map.empty
 where
  f s c m = foldr (\s' -> Map.insertWith (+) s' c) m $ step s

step :: Int -> [Int]
step 0 = [1]
step n
  | even len = map read [take k s, drop k s]
  | otherwise = [n * 2024]
 where
  s = show n
  len = length s
  k = len `div` 2