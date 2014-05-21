{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Within where

import GHC.TypeLits
--
import Control.Monad (guard)
import Data.Array
import Data.Proxy

newtype Within (n :: Nat) = MkWithin Integer 
                          deriving (Show, Eq, Ord, Enum, Ix) 

-- | 
mkWithin :: forall (n :: Nat). (KnownNat n) => Integer -> Maybe (Within n)
mkWithin v = guard (1 <= v && v <= (natVal (Proxy :: Proxy n))) >> return (MkWithin v)

-- |
mkWithinMod :: forall (n :: Nat) . (KnownNat n) => Integer -> Within n
mkWithinMod v = MkWithin v' 
  where nn = natVal (Proxy :: Proxy n)
        v' = let x = v `mod` nn in if x == 0 then nn else x

instance (KnownNat n) => Num (Within n) where
  (MkWithin a) + (MkWithin b) = mkWithinMod (a+b)
  (MkWithin a) * (MkWithin b) = mkWithinMod (a*b)
  abs (MkWithin a) = mkWithinMod (abs a)
  signum (MkWithin _) = MkWithin 1
  fromInteger a = mkWithinMod a
  negate (MkWithin a) = mkWithinMod (negate a)

-- |
interval :: forall p n. (KnownNat n) => p n -> [Within n]
interval x = [1..order x]

-- |
order :: forall p n. (KnownNat n) => p n -> Within n
order _ = let nn = natVal (Proxy :: Proxy n) in mkWithinMod nn

