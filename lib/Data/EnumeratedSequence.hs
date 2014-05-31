{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.EnumeratedSequence where

import GHC.TypeLits
--
import Control.Applicative (Alternative,Applicative)
import Control.Monad (MonadPlus)
-- import Data.Data (Data)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)
import Data.Traversable (Traversable)
import qualified Data.Sequence as S
-- import Data.Typeable (Typeable)

newtype NSeq (n :: Nat) a = NSeq {seq :: S.Seq a}
        
deriving instance Alternative (NSeq n)

deriving instance Monad (NSeq n) 

deriving instance Functor (NSeq n)

deriving instance MonadPlus (NSeq n) 

deriving instance Applicative (NSeq n)

deriving instance Foldable (NSeq n)

deriving instance Traversable (NSeq n)

deriving instance Monoid (NSeq n a)

-- deriving instance Typeable (NSeq n)

deriving instance (Eq a) => Eq (NSeq n a) 

-- deriving instance (Data a) => Data (NSeq n a)

deriving instance (Ord a) => Ord (NSeq n a)

deriving instance (Read a) => Read (NSeq n a)

deriving instance (Show a) => Show (NSeq n a)

empty :: NSeq 0 a
empty = NSeq S.empty

singleton :: a -> NSeq 1 a
singleton x = NSeq (S.singleton x)

(<|) :: a -> NSeq n a -> NSeq (n+1) a
x <| NSeq xs = NSeq (x S.<| xs)

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

(|>) :: NSeq n a -> a -> NSeq (n+1) a
NSeq xs |> x = NSeq (xs S.|> x)

(><) :: NSeq m a -> NSeq n a -> NSeq (m+n) a
NSeq xs >< NSeq ys = NSeq (xs S.>< ys)

data ViewL (n :: Nat) a where
  EmptyL :: ViewL 0 a
  (:<) :: a -> NSeq n a -> ViewL (n+1) a


viewl :: NSeq (n+1) a -> ViewL (n+1) a
viewl (NSeq s) = case S.viewl s of
                      S.EmptyL -> error "viewl: cannot happen"  -- guaranteed
                      x S.:< xs -> x :< (NSeq xs)
  
data ViewR (n :: Nat) a where
  EmptyR :: ViewR 0 a
  (:>) :: NSeq n a -> a -> ViewR (n+1) a

viewr :: NSeq (n+1) a -> ViewR (n+1) a
viewr (NSeq s) = case S.viewr s of
                      S.EmptyR -> error "viewr: cannot happen"  -- guaranteed
                      xs S.:> x -> NSeq xs :> x
  
   