module Y2024.D15 (main) where

import Solution
import Data.List.Split (splitOn)
import AoC.Coord
import Data.Map (Map)
import qualified Data.Map as M


main :: Solution m => m ()
main = do
  [grid, movesRaw] <- splitOn "\n\n" <$> getInput
  moves <- mapM charToVec $ concat $ lines movesRaw
  answer $ solve grid moves
  answer $ solve (widen =<< grid) moves

solve :: [Char] -> [Coord] -> Int
solve gridRaw = sum . M.mapWithKey gps
              . fst . foldl sim (grid,start)
 where
  gps (C y x) c = if c `elem` "O[" then 100*y + x else 0
  grid = M.fromList [(pt,x) | (pt,x) <- from2dString gridRaw, x /= '.']
  start = head [pt | (pt,'@') <- M.assocs grid]

sim :: (Map Coord Char, Coord) -> Coord -> (Map Coord Char, Coord)
sim (grid, here) move =
  case go M.empty [here] of
    Nothing    -> (grid , here)
    Just moved -> (grid', here+move)
     where
      grid' = M.union (M.mapKeysMonotonic (move +) moved)
                      (M.difference grid moved)
 where
  go seen [] = Just seen
  go seen (pt:pts)
    | M.notMember pt seen
    , Just c <- M.lookup pt grid
    = if c == '#' then Nothing else
      go (M.insert pt c seen)
         ([pt + east | yVal move /= 0, c == '['] ++
          [pt + west | yVal move /= 0, c == ']'] ++
          [pt + move] ++ pts)
    | otherwise = go seen pts

widen :: Char -> String
widen = \case
  '#' -> "##"
  'O' -> "[]"
  '.' -> ".."
  '@' -> "@."
  x   -> [x]
