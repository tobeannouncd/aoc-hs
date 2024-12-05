{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module AoC.Coord (
  pattern C,
  Coord,
  xVal,
  yVal,
  boundingBox,
  drawPicture,
  drawCoords,
  above,
  below,
  left,
  right,
  clockwise,
  counterclockwise,
  manhattan,
  norm1,
  neighbors,
  cardinal,
  origin,
  north,
  west,
  south,
  east,
  from2dString,
) where

import Data.Foldable (toList)
import Data.Map (Map)
import Data.Semigroup (Max (Max), Min (Min))
import Linear.V2 (V2 (..))

import Data.Map.Strict qualified as Map

type Coord = V2 Int

pattern C :: Int -> Int -> Coord
pattern C{yVal, xVal} = V2 yVal xVal

{-# COMPLETE C #-}

newtype Bounds = B {getBounds :: Maybe (Coord, Coord)}
  deriving (Semigroup, Monoid) via Maybe (V2 (Min Int), V2 (Max Int))

boundingBox :: (Foldable t) => t Coord -> Maybe (Coord, Coord)
boundingBox = getBounds . foldMap (\c -> B $ Just (c, c))

drawPicture :: Char -> Map Coord Char -> String
drawPicture bg pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C ylo xlo, C yhi xhi) ->
      unlines
        [[at $ C y x | x <- [xlo .. xhi]] | y <- [ylo .. yhi]]
 where
  at c = Map.findWithDefault bg c pixels

drawCoords :: (Foldable t) => t Coord -> String
drawCoords cs = drawPicture '·' $ Map.fromList [(c, '█') | c <- toList cs]

above :: Coord -> Coord
above (C y x) = C (y - 1) x

below :: Coord -> Coord
below (C y x) = C (y + 1) x

left :: Coord -> Coord
left (C y x) = C y (x - 1)

right :: Coord -> Coord
right (C y x) = C y (x + 1)

clockwise :: Coord -> Coord
clockwise (C y x) = C x (-y)

counterclockwise :: Coord -> Coord
counterclockwise (C y x) = C (-x) y

manhattan :: Coord -> Coord -> Int
manhattan a b = norm1 (a - b)

norm1 :: Coord -> Int
norm1 = sum . fmap abs

cardinal :: Coord -> [Coord]
cardinal c = [above c, left c, right c, below c]

neighbors :: Coord -> [Coord]
neighbors c =
  [ above (left c)
  , above c
  , above (right c)
  , left c
  , right c
  , below (left c)
  , below c
  , below (right c)
  ]

origin :: Coord
origin = C 0 0

north :: Coord
north = C (-1) 0

south :: Coord
south = C 1 0

west :: Coord
west = C 0 (-1)

east :: Coord
east = C 0 1

from2dString :: String -> [(Coord, Char)]
from2dString str =
  [ (C y x, c)
  | (y, row) <- zip [0 ..] (lines str)
  , (x, c) <- zip [0 ..] row
  ]