{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.EnumeratedSequence where

import GHC.TypeLits
--
import Control.Applicative
-- import Control.Monad (MonadPlus)
-- import Data.Data (Data)
import qualified Data.Foldable as F
-- import Data.Monoid (Monoid)
import Data.Traversable
import qualified Data.Sequence as S
import           Data.Proxy
-- import Data.Typeable (Typeable)

data NSeq (n :: Nat) a where
  NSeq :: Proxy n -> S.Seq a -> NSeq n a

instance Functor (NSeq n) where
  fmap f (NSeq prxy s) = NSeq prxy (fmap f s)        

instance F.Foldable (NSeq n) where
  foldr f z (NSeq _ s) = F.foldr f z s

instance Applicative (NSeq n) where
  pure x = NSeq (Proxy :: Proxy n) (pure x)
  (NSeq _ f) <*> (NSeq prxy x) = NSeq prxy (f <*> x)

instance Traversable (NSeq n) where
  sequenceA (NSeq prxy s) = NSeq prxy <$> sequenceA s

{- 
deriving instance Alternative (NSeq n)

deriving instance Monad (NSeq n) 

deriving instance Functor (NSeq n)

deriving instance MonadPlus (NSeq n) 



deriving instance Traversable (NSeq n)

deriving instance Monoid (NSeq n a)

-- deriving instance Typeable (NSeq n)

deriving instance (Eq a) => Eq (NSeq n a) 

-- deriving instance (Data a) => Data (NSeq n a)

deriving instance (Ord a) => Ord (NSeq n a)

deriving instance (Read a) => Read (NSeq n a)

deriving instance (Show a) => Show (NSeq n a)
-} 


empty :: NSeq 0 a
empty = NSeq (Proxy :: Proxy 0) S.empty

singleton :: a -> NSeq 1 a
singleton x = NSeq (Proxy :: Proxy 1) (S.singleton x)

(<|) :: a -> NSeq n a -> NSeq (n+1) a
x <| NSeq _ xs = NSeq (Proxy :: Proxy (n+1)) (x S.<| xs)

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

(|>) :: forall n a. NSeq n a -> a -> NSeq (n+1) a
NSeq _ xs |> x = NSeq (Proxy :: Proxy (n+1)) (xs S.|> x)

(><) :: forall m n a. NSeq m a -> NSeq n a -> NSeq (m+n) a
NSeq _ xs >< NSeq _ ys = NSeq (Proxy :: Proxy (m+n)) (xs S.>< ys)

data ViewL (n :: Nat) a where
  EmptyL :: ViewL 0 a
  (:<) :: a -> NSeq n a -> ViewL (n+1) a


viewl :: forall a n. NSeq (n+1) a -> ViewL (n+1) a
viewl (NSeq _ s) = case S.viewl s of
                     S.EmptyL -> error "viewl: cannot happen"  -- guaranteed
                     x S.:< xs -> x :< (NSeq (Proxy :: Proxy n) xs)
  
data ViewR (n :: Nat) a where
  EmptyR :: ViewR 0 a
  (:>) :: NSeq n a -> a -> ViewR (n+1) a

viewr :: forall a n. NSeq (n+1) a -> ViewR (n+1) a
viewr (NSeq _ s) = case S.viewr s of
                      S.EmptyR -> error "viewr: cannot happen"  -- guaranteed
                      xs S.:> x -> NSeq (Proxy :: Proxy n) xs :> x
  
   