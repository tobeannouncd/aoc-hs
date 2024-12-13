{-# LANGUAGE MonadComprehensions #-}
module Y2024.D13 (main) where

import Solution
import AoC.Parsec
import Data.Maybe (mapMaybe)
import Linear

type Dist  = Int
type Input = V3 (V2 Dist)

inputP :: Parser Input
inputP = V3 <$> button <*> button <*> prize
 where
  button =   V2 <$ (string "Button " *> satisfy (`elem` "AB") *> string ": X+")
         <*> int <* string ", Y+"
         <*> int <* newline
  prize  =   V2 <$ string "Prize: X="
         <*> int <* string ", Y="
         <*> int <* newline

main :: Solution m => m ()
main = do
  input <- parse' (sepEndBy1 inputP newline) =<< getInput
  answer $ sum $ mapMaybe winCost input
  answer $ sum $ mapMaybe (winCost . incr 10000000000000) input

incr :: Dist -> Input -> Input
incr n (V3 a b c) = V3 a b (pure n + c)

winCost :: Input -> Maybe Dist
winCost (V3 buttonA buttonB target)
  = [ sum $ sol *! V2 (V2 3 0) (V2 0 1) | mat !* sol == target ]
 where
  mat  = transpose $ V2 buttonA buttonB
  mat' = (fromIntegral <$>) <$> mat
  sol  = round <$> inv22 mat' !* tgt
  tgt  = fromIntegral <$> target :: V2 Double