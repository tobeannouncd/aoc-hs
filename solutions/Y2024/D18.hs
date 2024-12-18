module Y2024.D18 (main) where

import Solution
import AoC.Parsec
import AoC.Coord
import Data.Ix
import qualified Data.Set as S
import AoC.Search

start, end :: Coord
start = 0
end = 70

inputP :: Parser Coord
inputP = flip C <$> nat <* char ',' <*> nat

main :: Solution m => m ()
main = do
  input <- parse' (sepEndBy1 inputP newline) =<< getInput
  let next pt = let corrupt = S.fromList (take 1024 input) in
        [ AStep pt' 1 (manhattan pt' end)
        | pt' <- cardinal pt, inRange (start,end) pt', pt' `S.notMember` corrupt ]
  answer $ head [cost | (pt,cost) <- astar next start, pt == end]

  let next' n pt = let corrupt = S.fromList (take n input) in
        [pt' | pt' <- cardinal pt, inRange (start,end) pt', pt' `S.notMember` corrupt]
      nStop = head [n | n <- [1025..], end `notElem` dfs (next' n) start] - 1
      C yS xS = input !! nStop
  answerStrLn $ show xS ++ "," ++ show yS
