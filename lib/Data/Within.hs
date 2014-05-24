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
import Data.Hashable
import Data.Proxy

newtype Within (n :: Nat) = MkWithin { intValue :: Int } 
                          deriving (Eq, Ord, Enum, Ix) 

instance Show (Within n) where
  show (MkWithin i) = show i


-- |
interval :: (KnownNat n) => [Within n] 
interval = [1..order]

-- |
order :: forall n. (KnownNat n) => Within n 
order = let nn = (fromInteger . natVal) (Proxy :: Proxy n) in mkWithinMod nn


instance (KnownNat n) => Hashable (Within n) where
  hashWithSalt salt (MkWithin x) = hashWithSalt salt x 

-- | 
mkWithin :: forall (n :: Nat). (KnownNat n) => Int -> Maybe (Within n)
mkWithin v = guard (1 <= v && v <= (fromInteger . natVal) (Proxy :: Proxy n)) 
             >> return (MkWithin v)

-- |
mkWithinMod :: forall (n :: Nat) . (KnownNat n) => Int -> Within n
mkWithinMod v = MkWithin v' 
  where nn = (fromInteger . natVal) (Proxy :: Proxy n)
        v' = let x = v `mod` nn in if x == 0 then nn else x

instance (KnownNat n) => Num (Within n) where
  (MkWithin a) + (MkWithin b) = mkWithinMod (a+b)
  (MkWithin a) * (MkWithin b) = mkWithinMod (a*b)
  abs (MkWithin a) = mkWithinMod (abs a)
  signum (MkWithin _) = MkWithin 1
  fromInteger a = mkWithinMod (fromInteger a)
  negate (MkWithin a) = mkWithinMod (negate a)

