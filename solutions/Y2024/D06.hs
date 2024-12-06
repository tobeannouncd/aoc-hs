module Y2024.D06 (main) where

import AoC.Coord
import Data.Containers.ListUtils (nubOrd)
import Data.List (unfoldr)
import Data.Map (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Solution

main :: (Solution m) => m ()
main = do
  (guardPos, facing, tileMap) <- process . from2dString <$> getInput
  let guardPath = unfoldr (walk tileMap) start
      start = (guardPos, facing)
  answer $ length $ nubOrd $ map fst guardPath
  -- | I know this isn't efficient. I'll optimize later.
  answer $
    length
      [ ()
      | (pt, Open) <- Map.toList tileMap
      , let tileMap' = Map.insert pt Obstacle tileMap
      , isLoop $ unfoldr (walk tileMap') start
      ]

isLoop :: [(Coord, Coord)] -> Bool
isLoop = go S.empty
 where
  go _ [] = False
  go seen (here : rest) =
    here `S.member` seen
      || go (S.insert here seen) rest

walk :: Map Coord Tile -> (Coord, Coord) -> Maybe ((Coord, Coord), (Coord, Coord))
walk tileMap = go
 where
  go (here, facing) =
    if here `Map.notMember` tileMap
      then Nothing
      else Just ((here, facing), next)
   where
    next = f surround
    surround =
      [ (here + x, x)
      | x <- [facing, clockwise facing, -facing, counterclockwise facing]
      ]
    f (x@(there, _) : nxt) =
      case tileMap !? there of
        Just Obstacle -> f nxt
        _ -> x
    f _ = undefined

data Tile
  = Obstacle
  | Open
  deriving (Eq, Show)

process :: [(Coord, Char)] -> (Coord, Coord, Map Coord Tile)
process xs = (guardPos, facing, tileMap)
 where
  (guardPos, guardChar) = head [t | t <- xs, snd t `elem` "^v<>"]
  facing =
    case guardChar of
      '>' -> east
      '<' -> west
      '^' -> north
      'v' -> south
      _ -> error "no match"
  tileMap =
    Map.fromList
      [ (c, if v == '#' then Obstacle else Open)
      | (c, v) <- xs
      ]