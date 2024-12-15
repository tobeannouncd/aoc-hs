{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
module AoC.Box where

import AoC.Nat
import Control.Monad (foldM)
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.List (foldl1')
import Text.Read (Lexeme (..), lift, parens, prec, readPrec, step)
import Text.Read.Lex (expect)

type Box' n = Box (FromNatural n)

data Box :: Nat -> Type where
  Pt  :: Box Z
  Dim :: !Int  {- ^ inclusive lower bound -} ->
         !Int  {- ^ exclusive upper bound -} ->
         Box n {- ^ lower dimensional box -} ->
         Box (S n)

deriving instance Show (Box n)
deriving instance Eq (Box n)
deriving instance Ord (Box n)

size :: Box n -> Int
size Pt              = 1
size (Dim lo hi box) = (hi - lo) * size box

intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox Pt Pt = Just Pt
intersectBox (Dim a b xs) (Dim c d ys) =
  [ Dim x y zs
  | let x = max a c
        y = min b d
  , x < y
  , zs <- intersectBox xs ys
  ]

intersectBoxes :: [Box n] -> Maybe (Box n)
intersectBoxes []     = error "intersectBoxes: empty intersection"
intersectBoxes (x:xs) = foldM intersectBox x xs

-- | Follows the semantics of `subtract`, where the first box is subtracted from
--   the second.
subtractBox :: Box n -> Box n -> [Box n]
subtractBox b1 b2 = maybe [b2] (`subtractBox'` b2) $ intersectBox b1 b2

subtractBox' :: Box n -> Box n -> [Box n]
subtractBox' Pt Pt = []
subtractBox' (Dim a b xs) (Dim c d ys) =
     [Dim c a ys | c < a]
  ++ [Dim a b zs | zs <- subtractBox' xs ys]
  ++ [Dim b d ys | b < d]

coverBox :: Box n -> Box n -> Box n
coverBox (Dim a b x) (Dim c d y) = Dim (min a c) (max b d) (coverBox x y)
coverBox Pt Pt = Pt

coverBoxes :: [Box n] -> Box n
coverBoxes = foldl1' coverBox

unionBoxes :: [Box n] -> [Box n]
unionBoxes = foldr add []
 where
  add box rest = box : concatMap (subtractBox box) rest

instance UnfoldNat n => Read (Box n) where
  readPrec = getCompose (unfoldNat pt dim)
   where
    pt = Compose (parens $ Pt <$ (lift . expect $ Ident "Pt"))
    dim (Compose more) = Compose . parens . prec 10 $ do
      lift . expect $ Ident "Dim"
      a <- step readPrec
      b <- step readPrec
      x <- step more
      return (Dim a b x)