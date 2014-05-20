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
listFromInterval :: forall s e . (KnownNat s, KnownNat e) => NatInterval s e -> [Integer]
listFromInterval (NatInterval n') = let n = natVal n'
                                    in take (fromInteger n) $ iterate succ (natVal (Proxy :: Proxy s)) 

-- | 
data Vector n a where
   Nil :: Vector 0 a
   Cons :: forall a n. a -> Vector n a -> Vector (n+1) a

-- |
listFromVector :: Vector n a -> [a]
listFromVector Nil = []
listFromVector (x `Cons` xs) = x : listFromVector xs

-- |
data RegArray n a = RArray (RegInterval n) (Array Integer a)


-- | 
mkRegArray :: (KnownNat n) => Vector n a -> RegArray n a
mkRegArray = let ix = mkNatInterval
             in RArray ix . listArray (start ix, end ix) . listFromVector 

-- |
(^!) :: forall i n a b. (KnownNat n) => RegArray n a -> NumberInInterval i 1 n -> a 
(RArray _ arr) ^! (NumberInInterval p _ ) 
    = let nn = natVal (Proxy :: Proxy n) 
          ii = natVal p
      in arr ! ii


data NumberInInterval x s e where
  NumberInInterval :: forall x s e . (KnownNat s, KnownNat e, KnownNat x, s <= x , x <= e) =>
                      Proxy x -> NatInterval s e -> NumberInInterval x s e






-- | 
data Color = Red | Black

data Tree :: * -> Color -> Nat -> * where
  Leaf :: Tree a Black 0
  NodeR :: a -> Tree a Black n -> Tree a Black n -> Tree a Red n
  NodeB :: a -> Tree a c n    -> Tree a c' n   -> Tree a Black (n+1)


  