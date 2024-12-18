module Y2024.D17 (main) where

import Solution
import AoC.Parsec hiding (State)
import Data.List (intercalate)
import Data.Bits

inputP :: Parser (Int,Int,Int,[Int])
inputP = do
  a <- string "Register A: " *> int <* newline
  b <- string "Register B: " *> int <* newline
  c <- string "Register C: " *> int <* spaces
  prog <- string "Program: " *> sepBy1 nat (char ',')
  return (a,b,c,prog)

main :: Solution m => m ()
main = do
  (regA,regB,regC,prog) <- parse' inputP =<< getInput
  answerStrLn $ intercalate "," . map show $ run (Computer regA regB regC) prog
  answer $ minimum $ foldr (concatMap . part2 prog) [0] prog

{-| This solution can be used for all inputs that have the following properties:

* The program is a do-while loop which ends in a @JNZ 0@ instruction
* The values of registers B and C are not reused in the next iteration of the
  loop
* The value of register A is only modified once per loop with an @ADV 3@
  instruction
* The loop has a single @OUT 5@ instruction
-}
part2 :: [Int] -> Int -> Int -> [Int]
part2 prog expected prefix =
  [ a | x <- [0..7]
      , let a = prefix `shiftL` 3 .|. x
      , outVal (Computer a 0 0) prog == expected ]

outVal :: Computer -> [Int] -> Int
outVal comp prog = go comp prog
 where
  go c = \case
    0 : x : prog' -> go c{rA = rA c `shiftR` combo x} prog'
    1 : x : prog' -> go c{rB = rB c `xor` x} prog'
    2 : x : prog' -> go c{rB = 7 .&. combo x} prog'
    4 : _ : prog' -> go c{rB = rB c `xor` rC c} prog'
    5 : x : _ -> combo x .&. 7
    6 : x : prog' -> go c{rB = rA c `shiftR` combo x} prog'
    7 : x : prog' -> go c{rC = rA c `shiftR` combo x} prog'
    _ -> error $ "unexpected program format: " ++ show prog
   where
    combo x = [0, 1, 2, 3, rA c, rB c, rC c] !! x


data Computer = Computer {rA,rB,rC :: !Int}

run :: Computer -> [Int] -> [Int]
run comp prog = go comp prog
 where
  go c = \case
    0 : x : prog' -> go c{ rA = rA c `shiftR` combo x } prog'
    1 : x : prog' -> go c{ rB = rB c `xor`          x } prog'
    2 : x : prog' -> go c{ rB = 7    .&.      combo x } prog'
    3 : x : prog' -> go c (if rA c == 0 then prog' else drop x prog)
    4 : _ : prog' -> go c{ rB = rB c `xor`    rC c    } prog'
    5 : x : prog' -> combo x .&. 7 : go c prog'
    6 : x : prog' -> go c{ rB = rA c `shiftR` combo x } prog'
    7 : x : prog' -> go c{ rC = rA c `shiftR` combo x } prog'
    _           -> []
   where
    combo x = [0,1,2,3,rA c,rB c,rC c] !! x
