{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Permute where

import GHC.TypeLits
-- 
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (runEitherT, left)
import Data.Array
import Data.Array.ST 
import qualified Data.Foldable as F
import Data.Proxy

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

{- 
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
-}


data NumberInInterval x s e where
  NumberInInterval :: forall x s e . (KnownNat s, KnownNat e, KnownNat x, s <= x , x <= e) =>
                      Proxy x -> NatInterval s e -> NumberInInterval x s e


data NumberInIntervalBox s e = forall x. MkNumberInIntervalBox (NumberInInterval x s e) 

-- | 
data RevArray = RevArray { forwardArray :: Array Integer Integer
                         , backwardArray :: Array Integer Integer }

mkRevArray :: Array Integer Integer -> Either String RevArray
mkRevArray arr= runST action
  where action :: forall s. ST s (Either String RevArray)        
        action =   runEitherT $ do 
                     let (i1,i2) = bounds arr
                     rarr <- lift (newArray (i1,i2) Nothing :: ST s (STArray s Integer (Maybe Integer)))
                     rarr' <- lift (newArray_ (i1,i2) :: ST s (STArray s Integer Integer))
                     F.forM_ [i1..i2] $ \i -> do
                       let r = arr ! i
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not reversible"
                         Nothing -> lift (writeArray rarr r (Just i))
                     F.forM_ [i1..i2] $ \r -> 
                       maybe (left "not reversible") (\i -> lift (writeArray rarr' r i)) =<< lift (readArray rarr r)
                     return . RevArray arr =<< lift (freeze rarr')


newtype Permutation (n :: Nat) = Permutation { permmap :: RevArray }

mkPermutation :: forall n. (KnownNat n) => RevArray -> Either String (Permutation n)
mkPermutation revarr = let intval = mkNatInterval :: RegInterval n 
                           (i1,i2) = bounds (forwardArray revarr)
                       in if (i1,i2) == (start intval, end intval) 
                          then Right (Permutation revarr) 
                          else Left "interval mistmatch"

-- |
permuteForward :: forall i n a b. (KnownNat n) => Permutation n -> NumberInInterval i 1 n -> Integer
permuteForward p (NumberInInterval i _) = forwardArray (permmap p) ! (natVal i)

-- | 
permuteBackward :: forall j n a b. (KnownNat n) => Permutation n -> NumberInInterval j 1 n -> Integer
permuteBackward p (NumberInInterval j _) = backwardArray (permmap p) ! (natVal j)








-- | 
data Color = Red | Black

data RBTree :: * -> Color -> Nat -> * where
  RBLeaf :: RBTree a Black 0
  RBNodeR :: a -> RBTree a Black n -> RBTree a Black n -> RBTree a Red n
  RBNodeB :: a -> RBTree a c n    -> RBTree a c' n   -> RBTree a Black (n+1)



data Tree :: * -> * where
  Leaf :: a -> Tree a 
  Node :: Tree a -> Tree a -> Tree a


data TestType :: Tree Nat -> * where
  TestTyp1 :: TestType (Leaf 1)
  TestTyp2 :: TestType (Node (Leaf 2) (Leaf 1))

 