module Y2024.D11 (main) where

import Data.IntMap qualified as Map
import Solution

main :: (Solution m) => m ()
main = do
  stones <- Map.fromListWith (+) . map ((,1) . read) . words <$> getInput
  answer $ sum $ iterate blink' stones !! 25
  answer $ sum $ iterate blink' stones !! 75

blink' :: Map.IntMap Int -> Map.IntMap Int
blink' = Map.foldrWithKey f Map.empty
 where
  f s c m = foldr (\s' -> Map.insertWith (+) s' c) m $ blink s

blink :: Int -> [Int]
blink 0 = [1]
blink n
  | even len = map read [take k s, drop k s]
  | otherwise = [n * 2024]
 where
  s = show n
  len = length s
  k = len `div` 2