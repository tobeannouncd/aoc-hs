{-# LANGUAGE MonadComprehensions #-}
module Y2024.D13 (main) where

import Solution
import AoC.Parsec
import Data.Maybe (mapMaybe)
import Linear
import Data.Ix (Ix(..))
import Control.Monad (when, guard)

type Input = (V2 Int, V2 Int, V2 Int)

inputP :: Parser Input
inputP = (,,) <$> button 'A' <*> button 'B' <*> prize
 where
  button c =   V2 <$ (string "Button " *> char c *> string ": X+")
           <*> int <* string ", Y+"
           <*> int <* newline
  prize    =   V2 <$ string "Prize: X="
           <*> int <* string ", Y="
           <*> int <* newline

main :: Solution m => m ()
main = do
  input <- parse' (sepEndBy1 inputP newline) =<< getInput
  answer $ sum $ mapMaybe (winCost (Just 100)) input
  answer $ sum $ mapMaybe (winCost Nothing . incr 10000000000000) input

incr :: Int -> Input -> Input
incr n (a,b,t) = (a,b, pure n + t)

{-|
This function fails on inputs that have collinear button vectors. It is likely
that the inputs were designed to avoid this. It is also likely that the inputs
do not contain claw machines where the target can be reached in part 1 with
more than 100 button presses on either button.
-}
winCost :: Maybe Int -> Input -> Maybe Int
winCost limit (buttonA, buttonB, target) = do
  let mat  = transpose (V2 buttonA buttonB)
      mat' = (fromIntegral <$>) <$> mat
      tgt  = fromIntegral <$> target :: V2 Double
      sol  = round <$> luSolveFinite mat' tgt
  when (det22 mat == 0) $ error "Diophantus, is that you?"
  guard $ mat !* sol == target
  guard $ maybe True (\l -> inRange (0, pure l) sol) limit
  return $ sum $ sol *! scaled (V2 3 1)