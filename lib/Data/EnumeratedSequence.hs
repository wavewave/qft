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

newtype EnumSeq (n :: Nat) a = EnumSeq {seq :: S.Seq a}
        
deriving instance Alternative (EnumSeq n)

deriving instance Monad (EnumSeq n) 

deriving instance Functor (EnumSeq n)

deriving instance MonadPlus (EnumSeq n) 

deriving instance Applicative (EnumSeq n)

deriving instance Foldable (EnumSeq n)

deriving instance Traversable (EnumSeq n)

deriving instance Monoid (EnumSeq n a)

-- deriving instance Typeable (EnumSeq n)

deriving instance (Eq a) => Eq (EnumSeq n a) 

-- deriving instance (Data a) => Data (EnumSeq n a)

deriving instance (Ord a) => Ord (EnumSeq n a)

deriving instance (Read a) => Read (EnumSeq n a)

deriving instance (Show a) => Show (EnumSeq n a)

empty :: EnumSeq 0 a
empty = EnumSeq S.empty

singleton :: a -> EnumSeq 1 a
singleton x = EnumSeq (S.singleton x)

(<|) :: a -> EnumSeq n a -> EnumSeq (n+1) a
x <| EnumSeq xs = EnumSeq (x S.<| xs)

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

(|>) :: EnumSeq n a -> a -> EnumSeq (n+1) a
EnumSeq xs |> x = EnumSeq (xs S.|> x)

(><) :: EnumSeq m a -> EnumSeq n a -> EnumSeq (m+n) a
EnumSeq xs >< EnumSeq ys = EnumSeq (xs S.>< ys)

data ViewL (n :: Nat) a where
  EmptyL :: ViewL 0 a
  (:<) :: a -> EnumSeq n a -> ViewL (n+1) a


viewl :: EnumSeq (n+1) a -> ViewL (n+1) a
viewl (EnumSeq s) = case S.viewl s of
                      S.EmptyL -> error "viewl: cannot happen"  -- guaranteed
                      x S.:< xs -> x :< (EnumSeq xs)
  
data ViewR (n :: Nat) a where
  EmptyR :: ViewR 0 a
  (:>) :: EnumSeq n a -> a -> ViewR (n+1) a

viewr :: EnumSeq (n+1) a -> ViewR (n+1) a
viewr (EnumSeq s) = case S.viewr s of
                      S.EmptyR -> error "viewr: cannot happen"  -- guaranteed
                      xs S.:> x -> EnumSeq xs :> x
  
   