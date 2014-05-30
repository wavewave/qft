{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Fin1 where

import GHC.TypeLits
--
import Control.Monad (guard)
import Data.Array
import Data.Hashable
import Data.Proxy

newtype Fin1 (n :: Nat) = MkFin1 { intValue :: Int } 
                          deriving (Eq, Ord, Enum, Ix) 

type ℤ_ = Fin1

instance Show (Fin1 n) where
  show (MkFin1 i) = show i


-- |
interval :: (KnownNat n) => [Fin1 n] 
interval = [1..order]

-- |
order :: forall n. (KnownNat n) => Fin1 n 
order = let nn = (fromInteger . natVal) (Proxy :: Proxy n) in mkFin1Mod nn


instance (KnownNat n) => Hashable (Fin1 n) where
  hashWithSalt salt (MkFin1 x) = hashWithSalt salt x 

-- | 
mkFin1 :: forall (n :: Nat). (KnownNat n) => Int -> Maybe (Fin1 n)
mkFin1 v = guard (1 <= v && v <= (fromInteger . natVal) (Proxy :: Proxy n)) 
             >> return (MkFin1 v)

-- |
mkFin1Mod :: forall (n :: Nat) . (KnownNat n) => Int -> Fin1 n
mkFin1Mod v = MkFin1 v' 
  where nn = (fromInteger . natVal) (Proxy :: Proxy n)
        v' = let x = v `mod` nn in if x == 0 then nn else x

instance (KnownNat n) => Num (Fin1 n) where
  (MkFin1 a) + (MkFin1 b) = mkFin1Mod (a+b)
  (MkFin1 a) * (MkFin1 b) = mkFin1Mod (a*b)
  abs (MkFin1 a) = mkFin1Mod (abs a)
  signum (MkFin1 _) = MkFin1 1
  fromInteger a = mkFin1Mod (fromInteger a)
  negate (MkFin1 a) = mkFin1Mod (negate a)

