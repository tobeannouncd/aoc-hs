{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module AoC.Nat where

import GHC.TypeNats qualified as T

data Nat = Z | S Nat

type family FromNatural (n :: T.Natural) :: Nat where
  FromNatural 0 = Z
  FromNatural n = S (FromNatural (n T.- 1))

class UnfoldNat n where
  unfoldNat :: f Z -> (forall m. f m -> f (S m)) -> f n

instance UnfoldNat Z where
  unfoldNat z _ = z

instance UnfoldNat n => UnfoldNat (S n) where
  unfoldNat z s = s (unfoldNat z s)