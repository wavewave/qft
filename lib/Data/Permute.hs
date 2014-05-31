{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Permute where

import GHC.TypeLits
-- 
import           Control.Monad (when)
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (runEitherT, left, hoistEither)
import           Data.Array
import           Data.Array.ST 
import qualified Data.Foldable as F
import           Data.Hashable
-- import qualified Data.HashMap.Strict as HM
import           Data.List (partition,find)
-- import qualified Data.Map as M
import           Data.Maybe (fromJust, isNothing)
import           Data.STRef (newSTRef, readSTRef, writeSTRef) 

-- 
import Data.Fin1
import Util

type m :->  n = Array m n 

type (n :: Nat) ▸ (m :: Nat) = Z_ m :-> Z_ n  


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

-- |
data Perm (n :: Nat) = Perm { forward :: n ▸ n
                            , backward :: n ▸ n 
                            , firstUnfixed :: Maybe (Z_ n)
                            }
                     deriving (Show)

type S_ = Perm 

class FromTuple a where 
  type Tuple a :: * 
  fromTuple :: Tuple a -> Either String a

instance FromTuple (S_ 2) where
  type Tuple (S_ 2) = (Z_ 2, Z_ 2)
  fromTuple (a,b) = mkPerm (listArray (1,2) [a,b])

instance FromTuple (S_ 3) where
  type Tuple (S_ 3) = (Z_ 3, Z_ 3, Z_ 3)
  fromTuple (a,b,c) = mkPerm (listArray (1,3) [a,b,c])

instance FromTuple (S_ 4) where
  type Tuple (S_ 4) = (Z_ 4, Z_ 4, Z_ 4, Z_ 4)
  fromTuple (a,b,c,d) = mkPerm (listArray (1,4) [a,b,c,d])

instance FromTuple (S_ 5) where
  type Tuple (S_ 5) = (Z_ 5, Z_ 5, Z_ 5, Z_ 5, Z_ 5)
  fromTuple (a,b,c,d,e) = mkPerm (listArray (1,5) [a,b,c,d,e])

instance FromTuple (S_ 6) where
  type Tuple (S_ 6) = (Z_ 6, Z_ 6, Z_ 6, Z_ 6, Z_ 6, Z_ 6)
  fromTuple (a,b,c,d,e,f) = mkPerm (listArray (1,6) [a,b,c,d,e,f])

instance FromTuple (S_ 7) where
  type Tuple (S_ 7) = (Z_ 7, Z_ 7, Z_ 7, Z_ 7, Z_ 7, Z_ 7, Z_ 7)
  fromTuple (a,b,c,d,e,f,g) = mkPerm (listArray (1,7) [a,b,c,d,e,f,g])


instance (Ix i, Hashable a) => Hashable (Array i a) where
  hashWithSalt salt arr = hashWithSalt salt (elems arr)

instance (KnownNat n) => Hashable (S_ n) where 
  hashWithSalt salt (Perm f _b _k) = hashWithSalt salt f

-- |
mkPerm :: forall (n :: Nat) . (KnownNat n) => n ▸ n -> Either String (S_ n)
mkPerm arr= runST action
  where action :: forall s. ST s (Either String (S_ n))        
        action =   runEitherT $ do 
                     let (i1,i2) = bounds arr
                     hoistEither (guardEither "i1 is not 1" (i1 == 1))
                     mref <- lift (newSTRef (Nothing :: Maybe (Z_ n)))
                     rarr <- lift (newArray (i1,i2) Nothing :: ST s (STArray s (Z_ n) (Maybe (Z_ n))))
                     rarr' <- lift (newArray_ (i1,i2) :: ST s (STArray s (Z_ n) (Z_ n)))
                     F.forM_ [i1..i2] $ \i -> do
                       let r = arr ! i
                       mval <- lift (readSTRef mref)
                       when (r /= i && isNothing mval) $ lift (writeSTRef mref (Just i))
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not reversible"
                         Nothing -> lift (writeArray rarr r (Just i))
                     F.forM_ [i1..i2] $ \r -> 
                       maybe (left "not reversible") (\i -> lift (writeArray rarr' r i)) 
                         =<< lift (readArray rarr r)
                     rarr'' <- lift (freeze rarr')
                     mval <- lift (readSTRef mref)
                     return (Perm arr rarr'' mval)

 
-- |
permute :: S_ n -> Z_ n -> Z_ n
permute p i = forward p ! i

-- | synonym for permute
(↙) :: Z_ n -> S_ n -> Z_ n 
(↙) = flip permute    


-- | 
inverse :: S_ n -> S_ n 
inverse (Perm f b k) = Perm b f k

-- | synonym for inverse
(^-) :: S_ n -> S_ n
(^-) = inverse

-- |
mult :: ∀ n. S_ n -> S_ n -> S_ n 
mult p1 p2 = let (i1,i2) = bounds (forward p1)
                 flst = [ (i,r) | i <- [i1..i2], let r = forward p2 ! (forward p1 ! i) ]
                 blst = [ (i,r) | i <- [i1..i2], let r = backward p1 ! (backward p2 ! i) ]
                 kval = fmap fst (find (\(i,r) -> i /= r) flst)
                 farr = array (i1,i2) flst
                 barr = array (i1,i2) blst
             in Perm farr barr kval 

-- | synonym for mult
(·) :: S_ n -> S_ n -> S_ n 
(·) = mult

newtype Generator (k :: Nat) (n :: Nat) = Gen { unGen :: Z_ k :-> S_ n }

isIdentity :: (KnownNat n) => S_ n -> Bool
isIdentity = maybe True (const False) . firstUnfixed 
-- all (\i -> (forward p ! i) == i) interval

mkGen :: (KnownNat n) => (Z_ k :-> S_ n) -> Either String (Generator k n)
mkGen gen = if (any isIdentity (elems gen)) then Left "identity included" else Right (Gen gen)



isFixedBy :: Z_ n -> S_ n -> Bool
β `isFixedBy` g = β == (β ↙ g)

-- | result = (fixed, first unfixed, rest unfixed)
splitFixed :: (KnownNat n) => Generator k n -> ([Z_ n], Z_ n, [Z_ n])
splitFixed gen = let (intrvl1',intrvl2') = partition (\β -> (all (β `isFixedBy`) . elems . unGen) gen) intrvl2
                 in (intrvl1 ++ intrvl1', n1, intrvl2' )
  where ps = (elems . unGen) gen  
        n1 = (minimum . map (fromJust . firstUnfixed) ) ps 
        intrvl1 = if n1 == 1 then [] else [1..n1-1] 
        intrvl2 = if n1 == order then [] else [n1+1..order]


chooseUnfixed :: (KnownNat n) => Generator k n -> Z_ n 
chooseUnfixed = snd3 . splitFixed 

-- k is |generators|, n is degree

type Base (k :: Nat) (n :: Nat) = k ▸ n


newtype BSGS (k :: Nat) (n :: Nat) = BSGS (Base k n, Z_ k :-> [S_ n])

-- makeBSGSFromSGS :: Base k n -> H.HashSet (S_ n) -> BSGS k n 

-- | Schreier vector v is Omega -> {X}
newtype SchreierVector (k :: Nat) (n :: Nat) = SV (Z_ n :-> Maybe (Z_ k))

-- | Backward pointer omega is Omega -> Omega
newtype BackwardPointer (k :: Nat) (n :: Nat) = BP (Z_ n :-> Maybe (Z_ n))

-- | spanning tree is a pair of Schreier vector and backward pointer 
newtype SpanningTree (k :: Nat) (n :: Nat) = SpanTr ((SchreierVector k n, BackwardPointer k n))

-- | Schreier structure V is defined for BSGS
newtype SchreierStructure (k :: Nat) (n :: Nat) = SS ((Z_ k, Z_ n) :-> Maybe (Z_ k, Z_ n))

-- |
newtype PartialBSGS (k :: Nat) (n :: Nat) = PBSGS (Base k n, Z_ k :-> [S_ n])


-- trace 
-- orbit
-- sift
-- siftee
-- strip
-- permutationFromBaseImage (base impage to permutation)



