module AoC.Search (
  dfs, dfsN, dfsOn, dfsOnN,
  bfs, bfsN, bfsOn, bfsOnN,
) where
import qualified AoC.Queue as Q
import qualified Data.Set as Set

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id
{-# INLINE dfs #-}

dfsN :: Ord a => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id
{-# INLINE dfsN #-}

dfsOn :: Ord t => (a -> t) -> (a -> [a]) -> a -> [a]
dfsOn rep next = dfsOnN rep next . pure
{-# INLINE dfsOn #-}

dfsOnN :: Ord t => (a -> t) -> (a -> [a]) -> [a] -> [a]
dfsOnN rep next = loop Set.empty
 where
  loop !seen = \case
    [] -> []
    x:xs
      | Set.member r seen -> loop seen xs
      | otherwise -> x : loop seen' (next x ++ xs)
      where
        r = rep x
        seen' = Set.insert r seen

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id
{-# INLINE bfs #-}

bfsN :: Ord a => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id
{-# INLINE bfsN #-}

bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfsOn rep nxt = bfsOnN rep nxt . pure
{-# INLINE [0] bfsOn #-}

bfsOnN :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOnN rep next starts = loop Set.empty (Q.fromList starts)
 where
  loop !seen = \case
    Q.Empty -> []
    x Q.:<| q
      | Set.member r seen -> loop seen q
      | otherwise -> x : loop seen' q'
      where
        r = rep x
        seen' = Set.insert r seen
        q' = Q.appendList q (next x)