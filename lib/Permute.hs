{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
--
import Unsafe.Coerce

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

data XInInterval x s e where
  XInInterval :: forall x s e . (KnownNat s, KnownNat e, KnownNat x, s <= x , x <= e) =>
                 Proxy x -> XInInterval x s e


data XInIntervalBox (s :: Nat) (e :: Nat) where
  MkXInIntervalBox :: forall x s e. (KnownNat s, KnownNat e, KnownNat x, s <= x, x <= e) => 
                      XInInterval x s e -> XInIntervalBox s e

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

{- 
-- |
permuteForward :: forall i n a b.  
                  Permutation n 
               -> NumberInIntervalBox 1 n 
               -> NumberInIntervalBox 1 n
permuteForward p (MkNumberInIntervalBox (NumberInInterval i)) = 
    case someNatVal (forwardArray (permmap p) ! (natVal i)) of 
      Nothing -> error "impossible"
      Just (SomeNat (prxy :: Proxy j)) -> MkNumberInIntervalBox (NumberInInterval prxy)
-}

type family F (a :: Nat) :: Nat where
  F 0 = 1
  F 1 = 2
  F a = 3

class IsElemType x set where 
  isElemType :: x -> set -> Bool


instance (KnownNat n, KnownNat s, KnownNat e, s <= n, n <= e ) => IsElemType (Proxy n) (NatInterval s e) where
  isElemType _ _ = True

instance IsElemType (Proxy n) (NatInterval s e) where
  isElemType _ _ = False





-- | 
permuteBackward :: forall j n a b. (KnownNat n) => Permutation n -> XInInterval j 1 n -> Integer
permuteBackward p (XInInterval j) = backwardArray (permmap p) ! (natVal j)




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

 