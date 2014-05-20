{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad (guard)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (runEitherT, left, hoistEither)
import Data.Array
import Data.Array.ST 
import qualified Data.Foldable as F
import Data.Promotion.Prelude
import Data.Proxy

--
import Unsafe.Coerce

{- 
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

-}

guardEither :: String -> Bool -> Either String ()
guardEither str False = Left str
guardEither str True = Right ()

maybeEither :: String -> Maybe a -> Either String a
maybeEither str Nothing = Left str
maybeEither _ (Just x) = Right x

newtype Within (n :: Nat) = MkWithin Integer 
                          deriving (Show, Eq, Ord, Enum, Ix) 



mkWithin :: forall (n :: Nat). (KnownNat n) => Integer -> Maybe (Within n)
mkWithin v = guard (1 <= v && v <= (natVal (Proxy :: Proxy n))) >> return (MkWithin v)



mkWithinMod :: forall (n :: Nat) . (KnownNat n) => Integer -> Within n
mkWithinMod v = MkWithin v' 
  where nn = natVal (Proxy :: Proxy n)
        v' = let x = v `mod` nn in if x == 0 then nn else x

instance (KnownNat n) => Num (Within n) where
  (MkWithin a) + (MkWithin b) = mkWithinMod (a+b)
  (MkWithin a) * (MkWithin b) = mkWithinMod (a*b)
  abs (MkWithin a) = mkWithinMod (abs a)
  signum (MkWithin a) = MkWithin 1
  fromInteger a = mkWithinMod a
  negate (MkWithin a) = mkWithinMod (negate a)



is1 :: Within n -> Bool 
is1 (MkWithin v) = v == 1

             
-- |
data Permutation (n :: Nat) = Permutation { forward :: Array (Within n) (Within n)
                                          , backward :: Array (Within n)  (Within n) }
                            deriving (Show)

mkPermutation :: forall (n :: Nat) . (KnownNat n) => Array (Within n) (Within n) -> Either String (Permutation n)
mkPermutation arr= runST action
  where action :: forall s. ST s (Either String (Permutation n))        
        action =   runEitherT $ do 
                     let (i1,i2) = bounds arr
                     hoistEither (guardEither "i1 is not 1" (is1 i1))
                     rarr <- lift (newArray (i1,i2) Nothing :: ST s (STArray s (Within n) (Maybe (Within n))))
                     rarr' <- lift (newArray_ (i1,i2) :: ST s (STArray s (Within n) (Within n)))
                     F.forM_ [i1..i2] $ \i -> do
                       let r = arr ! i
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not reversible"
                         Nothing -> lift (writeArray rarr r (Just i))
                     F.forM_ [i1..i2] $ \r -> 
                       maybe (left "not reversible") (\i -> lift (writeArray rarr' r i)) =<< lift (readArray rarr r)
                     return . Permutation arr =<< lift (freeze rarr')

{- 
newtype Permutation (n :: Nat) = Permutation { permmap :: RevArray }

mkPermutation :: forall n. (KnownNat n) => RevArray -> Either String (Permutation n)
mkPermutation revarr = if (1, natVal (Proxy :: Proxy n)) == bounds (forwardArray revarr)
                           then Right (Permutation revarr) 
                           else Left "interval mistmatch"
-}


 
-- |
permute :: Permutation n -> Within n -> Within n
permute p i = forward p ! i

-- | 
invperm :: Permutation n -> Within n -> Within n
invperm p i = backward p ! i


{-
 
    case someNatVal (forwardArray (permmap p) ! (natVal i)) of 
      Nothing -> error "impossible"
      Just (SomeNat (prxy :: Proxy j)) -> let b = boolean (Proxy :: Proxy (F (1 <=? n))) in undefined 

                                          undefined 

class BooleanKindToType a where 
  boolean :: a -> Bool 

type family F (a :: Bool) :: * where
  F True = ()
  F False = ()

instance BooleanKindToType (Proxy ()) where boolean _ = True

instance BooleanKindToType (Proxy True) where
  boolean _ = True

instance BooleanKindToType (Proxy False) where
  boolean _ = False

-}

-- MkNumberInIntervalBox (NumberInInterval prxy)

{- 
 
class IsElemType x set where 
  isElemType :: x -> set -> Bool


instance (KnownNat n, KnownNat s, KnownNat e, s <= n, n <= e ) => IsElemType (Proxy n) (NatInterval s e) where
  isElemType _ _ = True

instance IsElemType (Proxy n) (NatInterval s e) where
  isElemType _ _ = False





-- | 
permuteBackward :: forall j n a b. (KnownNat n) => Permutation n -> XInInterval j 1 n -> Integer
permuteBackward p (XInInterval j) = backwardArray (permmap p) ! (natVal j)


-}