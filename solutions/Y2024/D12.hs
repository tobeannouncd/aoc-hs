module Y2024.D12 (main) where

import Solution
import qualified Data.Map as Map
import AoC.Coord
import Data.Map (Map, (!?))
import qualified Data.Set as Set
import AoC.Search (dfs)
import Data.List ((\\))
import Control.Monad (guard, void)
import AoC (countIf)

main :: Solution m => m ()
main = do
  garden <- Map.fromList . from2dString <$> getInput
  let regions = findRegions garden
  answer $ sum $ map ((*) <$> length <*> perimeter) regions
  answer $ sum $ map ((*) <$> length <*> sides) regions

findRegions :: Map Coord Char -> [[Coord]]
findRegions garden = go Set.empty (Map.keys garden)
 where
  go _ [] = []
  go seen (start:rest) =
    let region = dfs next start
        seen'  = foldr Set.insert seen region
        rest'  = rest \\ region
    in region : go seen' rest'
  next here =
    [ there
    | there <- cardinal here
    , garden !? here == garden !? there ]

perimeter :: [Coord] -> Int
perimeter region = countIf (`notElem` region) $ concatMap cardinal region

sides :: [Coord] -> Int
sides region = sum $ map (length . corners) region
 where
  corners pt = do
    (a,b) <- [(above,left),(left,below),(below,right),(right,above)]
    let edges = [a pt, b pt]
    void . guard $ if a (b pt) `elem` region
      then all (`notElem` region) edges
      else (a pt `elem` region) == (b pt `elem` region)