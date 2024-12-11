module Y2024.D09 (main) where

import Solution
import Data.Char
import AoC (countIf)
import Data.Maybe (isJust, isNothing)
import Data.List (group)
import Control.Monad (void)

main :: (Solution m) => m ()
main = do
  input <- map digitToInt . head . lines <$> getInput
  let disk = unpack input
  answer $ checksum $ part1 disk
  answer $ checksum $ part2 disk

checksum :: [Maybe Int] -> Int
checksum = sum . zipWith (\i -> maybe 0 (i*)) [0..]

unpack :: [Int] -> [Maybe Int]
unpack = go 0
 where
  go _ [] = []
  go i [x] = replicate x (Just i)
  go i (x:y:ys) = replicate x (Just i) ++ replicate y Nothing ++ go (succ i) ys

part1,part2 :: [Maybe Int] -> [Maybe Int]
part1 xs = take len $ go xs $ reverse (filter isJust xs)
 where
  len = countIf isJust xs
  go ys [] = ys
  go (Nothing:ys) (z:zs) = z:go ys zs
  go (y:ys) zs = y:go ys zs
  go [] _ = []
part2 disk
  | Just n <- last disk = concat $ foldr move (group disk) [0..n]
  | otherwise = undefined
  where
    move i grps =
      case break ((== [Just i]) . take 1) grps of
        (xs, g:ys) -> let lenG = length g in
          case break (\x -> void x >= void g && isNothing (head x)) xs of
            (xs', x:xs'') -> xs' ++ g : drop lenG x : xs'' ++ replicate lenG Nothing : ys
            _ -> grps
        _ -> undefined