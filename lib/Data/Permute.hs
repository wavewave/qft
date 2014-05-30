{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Permute where

import GHC.TypeLits
-- 
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (runEitherT, left, hoistEither)
import Data.Array
import Data.Array.ST 
import qualified Data.Foldable as F
import Data.Hashable
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Map as M

-- 
import Data.Fin1
import Util
        
-- |
data Perm (n :: Nat) = Perm { forward :: Array (Fin1 n) (Fin1 n)
                            , backward :: Array (Fin1 n)  (Fin1 n) }
                     deriving (Show)


instance (Ix i, Hashable a) => Hashable (Array i a) where
  hashWithSalt salt arr = hashWithSalt salt (elems arr)

instance (KnownNat n) => Hashable (Perm n) where 
  hashWithSalt salt (Perm f b) = hashWithSalt salt (f,b)

-- |
mkPerm :: forall (n :: Nat) . (KnownNat n) => Array (Fin1 n) (Fin1 n) -> Either String (Perm n)
mkPerm arr= runST action
  where action :: forall s. ST s (Either String (Perm n))        
        action =   runEitherT $ do 
                     let (i1,i2) = bounds arr
                     hoistEither (guardEither "i1 is not 1" (i1 == 1))
                     rarr <- lift (newArray (i1,i2) Nothing :: ST s (STArray s (Fin1 n) (Maybe (Fin1 n))))
                     rarr' <- lift (newArray_ (i1,i2) :: ST s (STArray s (Fin1 n) (Fin1 n)))
                     F.forM_ [i1..i2] $ \i -> do
                       let r = arr ! i
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not reversible"
                         Nothing -> lift (writeArray rarr r (Just i))
                     F.forM_ [i1..i2] $ \r -> 
                       maybe (left "not reversible") (\i -> lift (writeArray rarr' r i)) =<< lift (readArray rarr r)
                     return . Perm arr =<< lift (freeze rarr')

 
-- |
permute :: Perm n -> Fin1 n -> Fin1 n
permute p i = forward p ! i

-- | 
inverse :: Perm n -> Perm n 
inverse (Perm f b) = Perm b f

-- |
mult :: forall n. Perm n -> Perm n -> Perm n 
mult p1 p2 = Perm f b 
  where (f,b) = let (i1,i2) = bounds (forward p1)
                    farr = listArray (i1,i2) [ r | i <- [i1..i2], let r = forward p2 ! (forward p1 ! i) ]
                    barr = listArray (i1,i2) [ r | i <- [i1..i2], let r = backward p1 ! (backward p2 ! i) ]
                in (farr,barr)


-- k is |generators|, n is degree

type Base (k :: Nat) (n :: Nat) = Array (Fin1 k) (Fin1 n) 

type Generators (k :: Nat) (n :: Nat) =  Array (Fin1 k) (Perm n)

type BSGS (k :: Nat) (n :: Nat) = (Base k n, Array (Fin1 k) [Perm n])

-- makeBSGSFromSGS :: Base k n -> H.HashSet (Perm n) -> BSGS k n 



-- | Schreier vector v is Omega -> {X}
newtype SchreierVector (k :: Nat) (n :: Nat) = SV (Array (Fin1 n) (Maybe (Fin1 k)))

-- | Backward pointer omega is Omega -> Omega
newtype BackwardPointer (k :: Nat) (n :: Nat) = BP (Array (Fin1 n) (Maybe (Fin1 n)))

-- | spanning tree is a pair of Schreier vector and backward pointer 
newtype SpanningTree (k :: Nat) (n :: Nat) = ST ((SchreierVector k n, BackwardPointer k n))

-- | Schreier structure V is defined for BSGS
newtype SchreierStructure (k :: Nat) (n :: Nat) = SS (Array (Fin1 k,Fin1 n) (Maybe (Fin1 k,Fin1 n)))



-- trace 
-- orbit
-- sift
-- siftee
-- strip
-- permutationFromBaseImage (base impage to permutation)



