module Y2024.D15 (main) where

import Solution
import Data.List.Split (splitOn)
import AoC.Coord
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

data Tile = Wall | Floor | Box | Robot | BoxL | BoxR
  deriving (Show, Eq)

toVec :: (MonadFail m) => Char -> m Coord
toVec = \case
  '<' -> return west
  '>' -> return east
  '^' -> return north
  'v' -> return south
  x   -> fail $ "toVec: bad char " ++ show x

main :: Solution m => m ()
main = do
  [grid, movesRaw] <- splitOn "\n\n" <$> getInput
  moves <- mapM toVec $ concat $ lines movesRaw
  answer $ solve grid moves
  answer $ solve (widen =<< grid) moves

solve :: [Char] -> [Coord] -> Int
solve grid = sum . map gps . boxes
           . fst . run (parse grid)
 where
  gps (C y x) = 100*y + x
  boxes = M.keys . M.filter (`elem` ['O','['])

parse :: String -> (Map Coord Char, Coord)
parse gridRaw = (grid, fst $ M.elemAt 0 start)
 where
  (start, grid) = M.partition (== '@') $
    M.fromList [(pt, x) | (pt,x) <- from2dString gridRaw, x /= '.']

run :: (Map Coord Char, Coord) -> [Coord] -> (Map Coord Char, Coord)
run = foldl \(chart, p) dir ->
  case toMove chart p dir of
    Nothing -> (chart, p)
    Just ps ->
      let psSet = S.fromList ps
      in (M.mapKeys (\p' -> if p' `S.member` psSet then p' + dir else p') chart, p + dir)

toMove :: Map Coord Char -> Coord -> Coord -> Maybe [Coord]
toMove grid = go
 where
  go here move =
    case M.findWithDefault '.' there grid of
      '.' -> Just [here]
      'O' -> (here:) <$> go there move
      '[' -> do
        r1 <- go there move
        r2 <- if horizontal then Just [] else go (right there) move
        return (here : r1 ++ r2)
      ']' -> do
        r1 <- go there move
        r2 <- if horizontal then Just [] else go (left there) move
        return (here : r1 ++ r2)
      _ -> Nothing
   where
    there = here + move
    horizontal = yVal move == 0

widen :: Char -> String
widen '#' = "##"
widen 'O' = "[]"
widen '.' = ".."
widen '@' = "@."
widen '\n' = "\n"
widen x = error $ "widen: bad char " ++ show x
