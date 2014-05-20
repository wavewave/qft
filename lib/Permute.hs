{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Proxy


guardEither :: String -> Bool -> Either String ()
guardEither str False = Left str
guardEither _ True = Right ()

maybeEither :: String -> Maybe a -> Either String a
maybeEither str Nothing = Left str
maybeEither _ (Just x) = Right x

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

 
-- |
permute :: Permutation n -> Within n -> Within n
permute p i = forward p ! i

-- | 
inverse :: Permutation n -> Permutation n 
inverse (Permutation f b) = Permutation b f

-- |
newtype OrderedPartition n = OP [ [Within n] ] 
                           deriving Show

mkOrderedPartition :: forall (n :: Nat) . (KnownNat n) => [ [ Within n ] ] -> Either String (OrderedPartition n)
mkOrderedPartition lst = runST action
  where nn = MkWithin (natVal (Proxy :: Proxy n))
        action :: forall s. ST s (Either String (OrderedPartition n))        
        action =   runEitherT $ do 
                     rarr <- lift (newArray (1,nn) Nothing :: ST s (STArray s (Within n) (Maybe ())))
                     F.forM_ (concat lst) $ \r -> do
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not a partition"
                         Nothing -> lift (writeArray rarr r (Just ()))
                     F.forM_ [1..nn] $ \r -> 
                       maybe (left "not a partition") (const (return ())) =<< lift (readArray rarr r)
                     (return . OP) lst




 

