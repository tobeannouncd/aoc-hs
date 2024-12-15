module Y2024.D14 (main) where

import Solution
import AoC.Parsec
import AoC.Coord
import Data.List (group, sort)
import Math.NumberTheory.Moduli (chinese)

isExample :: Bool
isExample = False

width,height :: Int
width  = if isExample then 11 else 101
height = if isExample then 7 else 103

type Robot = (Coord,Coord)

inputP :: Parser Robot
inputP =   (,) <$ string "p="
       <*> pt <* string " v="
       <*> pt
 where
  pt = flip C <$> int <* char ',' <*> int

main :: Solution m => m ()
main = do
  robots <- parse' (sepEndBy1 inputP newline) =<< getInput
  let part1 = map (wait 100) robots
      (part2, tree) = findTree robots
  answer $ safetyFactor part1
  answer (part2 :: Int)
  -- answerStr $ drawCoords tree
{- ^
······································█·····························································
···················█················································································
·············█·······································█··············································
······························█·····································································
···············································································█····················
·······█···········································█································█···············
························█·····································█································█····
··········································································█·························
····································································································
··█····················█·······································█······························█···█·
···············································································█····················
··█·················█··█············································································
········█······█··························█·············································█···········
····································································································
···················█················································································
··············█························█·····················································█······
···········█···················█····························································█·······
············█····················█··································································
···································································································█
································███████████████████████████████·····································
·············█··················█·····························█·····································
································█·····························█·····································
·························█······█·····························█·····································
·····················█·····█····█·····························█································█····
··················█·············█··············█··············█·····································
··············█······█··········█·············███·············█·····································
··········█·····················█············█████············█·····█·······························
································█···········███████···········█·························█······█····
································█··········█████████··········█····················█················
································█············█████············█························█············
·························█······█···········███████···········█······█······················█·······
································█··········█████████··········█·····································
································█·········███████████·········█····························█········
···········█····················█········█████████████········█·····································
·········█······················█··········█████████··········█····█································
································█·········███████████·········█·····································
································█········█████████████········█·····································
································█·······███████████████·······█·····································
································█······█████████████████······█··························█··········
·█················█·············█········█████████████········█·································█···
··························█·····█·······███████████████·······█·····································
····█···························█······█████████████████······█·····································
································█·····███████████████████·····█·····································
······█·············█···········█····█████████████████████····█··················█··················
································█·············███·············█·····································
······························█·█·············███·············█·····································
····························█···█·············███·············█·····································
································█·····························█···························█·········
································█·····························█·····································
································█·····························█·················█···················
································█·····························█········█··························█·
································███████████████████████████████·····································
·············································································█······················
···················█················································································
····█······························································································█
·························█····█··········█··············█······█········█······················█····
····································································································
··············································█·····················█·······························
····································································································
····································································································
······█·····························································································
····································································································
··············█·············································································█·······
····································································································
··································█·································································
·······················································································█············
···················································█················································
···························██·······································································
························█···········································································
····································································································
····················█·····················································█······················█··
············█········································█··············································
······█·····························································································
·································································································█··
····································································································
········█·······················································█·························█·········
····································································································
··············································································█·····················
····································································································
····································································································
····································································································
········█···························································································
···························█············█···········█························█······················
······█··················█·····█·························█·······················█··················
·························································································█·····█····
····························█··············································█························
····································································································
·····█···············█·················································█····························
····································································································
················█··········██·······································································
····································································································
················································█···················································
·························································································█········█·
············································································█·······················
·····························································█······································
······················█·······················█·····················································
············█···············································█·······································
█·····························█·····································································
············█·············█·········█·······························································
··························█·····█··············█·······························█····················
··············█·····················································································
······························█·····································································
···························█············································█···························
-}

-- | Finds the times that produce the lowest variance in X and Y individually,
--   and uses the Chinese Remainder Theorem to determine the time that produces
--   the minimum variance in X and Y simultaneously.
findTree :: [Robot] -> (Int, [Coord])
findTree robots =
  case chinese (ty, height) (tx, width) of
    Just (t,_) -> (t, steps !! t)
    _          -> error "PANIC"
 where
  steps = map (map fst) $ iterate (map (wait 1)) robots
  xVals = map (map xVal) $ take width steps
  yVals = map (map yVal) $ take height steps
  nBots = fromIntegral $ length robots :: Double
  var xs =
    let xs' = map fromIntegral xs
        mn  = sum xs' / nBots
    in sum [(x-mn)^(2 :: Int) | x <- xs'] / nBots
  ty = snd $ minimum $ zip (map var yVals) [0..]
  tx = snd $ minimum $ zip (map var xVals) [0..]



wait :: Int -> Robot -> Robot
wait secs (C y x,vel@(C vy vx)) = (C y' x', vel)
 where
  y' = (y + secs * vy) `mod` height
  x' = (x + secs * vx) `mod` width

safetyFactor :: [Robot] -> Int
safetyFactor = product
             . map length
             . group . sort
             . map quadrant
             . filter onQuad
             . map fst
 where
  quadrant (C y x) = (2*y<height, 2*x<width)
  onQuad (C y x) = 2*y + 1 /= height && 2*x+1 /= width
