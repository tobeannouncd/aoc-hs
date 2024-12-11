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
dfsOnN rep next = go Set.empty
 where
  go !seen = \case
    [] -> []
    x:xs
      | r `Set.member` seen -> go seen xs
      | otherwise -> x : go seen' (next x ++ xs)
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
bfsOnN rep next starts = go Set.empty (Q.fromList starts)
 where
  go !seen = \case
    Q.Empty -> []
    x Q.:<| q
      | r `Set.member` seen -> go seen q
      | otherwise -> x : go seen' q'
      where
        r = rep x
        seen' = Set.insert r seen
        q' = Q.appendList q (next x)