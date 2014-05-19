{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Permute where

-- import Data.Type.Equality
import GHC.TypeLits
import Data.Proxy
import Data.Array


data NatInterval s e where 
  NatInterval :: forall s e n . (KnownNat s, KnownNat e, KnownNat n, (n+s) ~ (e+1)) => 
                 (Proxy n) -> NatInterval s e

start :: forall s e . (KnownNat s) => NatInterval s e -> Integer
start _ = natVal (Proxy :: Proxy s) 

end :: forall s e . (KnownNat e) => NatInterval s e -> Integer
end _ = natVal (Proxy :: Proxy e)

type RegInterval = NatInterval 1

-- | 
mkNatInterval :: forall s e n . (KnownNat s, KnownNat e, KnownNat n, (n+s) ~ (e+1)) => 
                 NatInterval s e
mkNatInterval = NatInterval (Proxy :: Proxy n)


-- | 
regularizeInterval :: forall s e n . (KnownNat s, KnownNat e, KnownNat n, (n+s) ~ (e+1)) => 
                      NatInterval s e -> NatInterval 1 n
regularizeInterval (NatInterval n) = mkNatInterval


-- | 
listFromInterval :: forall s e n . 
                    (KnownNat s, KnownNat e, KnownNat n, (n+s) ~ (e+1)) => NatInterval s e -> [Integer]
listFromInterval (NatInterval n') = let n = natVal n'
                                    in take (fromInteger n) $ iterate succ (natVal (Proxy :: Proxy s)) 

data Vector n a where
   Nil :: Vector 0 a
   Cons :: forall a n. a -> Vector n a -> Vector (n+1) a

listFromVector :: Vector n a -> [a]
listFromVector Nil = []
listFromVector (x `Cons` xs) = x : listFromVector xs

data RegArray n a = RArray (RegInterval n) (Array Integer a)

mkRegArray :: (KnownNat n) => Vector n a -> RegArray n a
mkRegArray = let ix = mkNatInterval
             in RArray ix . listArray (start ix, end ix) . listFromVector 



(^!) :: forall a n . (KnownNat n) => RegArray n a -> Integer -> a 
(RArray _ arr) ^! i = let nn = natVal (Proxy :: Proxy n) 
                          ii' = i `mod` nn
                          ii = if ii' == 0 then nn else ii'
                      in arr ! ii

