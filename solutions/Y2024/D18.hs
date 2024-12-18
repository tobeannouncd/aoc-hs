module Y2024.D18 (main) where

import Solution
import AoC.Parsec
import AoC.Coord
import Data.Ix
import AoC.Search
import qualified Data.Map as M

start, end :: Coord
start = 0
end = 70

inputP :: Parser Coord
inputP = flip C <$> nat <* char ',' <*> nat

main :: Solution m => m ()
main = do
  input <- parse' (sepEndBy1 inputP newline) =<< getInput
  let corrupt = M.fromList $ zip input [0 :: Int ..]
      next pt =
        [ AStep pt' 1 (manhattan pt' end)
        | pt' <- cardinal pt, inRange (start,end) pt'
        , case corrupt M.!? pt' of
            Just i -> i >= 1024
            _      -> True
        ]
  answer $ head [cost | (pt,cost) <- astar next start, pt == end]

  let next' n pt =
        [ pt'
        | pt' <- cardinal pt, inRange (start,end) pt'
        , case corrupt M.!? pt' of
            Just i -> i > n
            _      -> True
        ]
      nStop = head [n | n <- [1024..], end `notElem` dfs (next' n) start]
      C yS xS = input !! nStop
  answerStrLn $ show xS ++ "," ++ show yS
