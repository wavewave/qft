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
import Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import Data.Monoid (Monoid)
import Data.Traversable
import qualified Data.Sequence as S
import           Data.Proxy
-- import Data.Typeable (Typeable)

{- 
data NSeq (n :: Nat) a where
  NSeq :: Proxy n -> S.Seq a -> NSeq n a

instance Functor (NSeq n) where
  fmap f (NSeq prxy s) = NSeq prxy (fmap f s)        

instance F.Foldable (NSeq n) where
  foldr f z (NSeq _ s) = F.foldr f z s

instance Applicative (NSeq n) where
  pure x = NSeq (Proxy :: Proxy n) (pure x)
  (NSeq _ f) <*> (NSeq prxy x) = NSeq  prxy (f <*> x)

instance Traversable (NSeq n) where
  sequenceA (NSeq prxy s) = NSeq prxy <$> sequenceA s
-}


newtype NSeq (n :: Nat) a = NSeq (S.Seq a)

deriving instance Functor (NSeq n)

deriving instance F.Foldable (NSeq n) 

deriving instance Applicative (NSeq n) 

deriving instance Traversable (NSeq n) 

deriving instance Alternative (NSeq n)

deriving instance Monad (NSeq n) 

deriving instance MonadPlus (NSeq n) 

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
infixr 5 <|
infixl 5 |>

(|>) :: forall n a. NSeq n a -> a -> NSeq (n+1) a
NSeq xs |> x = NSeq (xs S.|> x)

(><) :: forall m n a. NSeq m a -> NSeq n a -> NSeq (m+n) a
NSeq xs >< NSeq ys = NSeq (xs S.>< ys)

data IsZero :: Nat -> * where
  IsZero :: IsZero 0
  IsSucc :: Proxy n -> IsZero (n+1)

data ViewL (n :: Nat) a where
  EmptyL :: ViewL 0 a
  (:<) :: a -> NSeq n a -> ViewL (n+1) a

 
viewl :: forall a n. IsZero n -> NSeq n a -> ViewL n a
viewl IsZero _  = EmptyL
viewl (IsSucc _) (NSeq s) =  
    case S.viewl s of 
      S.EmptyL -> error "viewl: impossible" -- totality guaranteed. 
      x S.:< xs -> x :< NSeq xs

data ViewR (n :: Nat) a where
  EmptyR :: ViewR 0 a
  (:>) :: NSeq n a -> a -> ViewR (n+1) a

viewr :: forall a n. IsZero n -> NSeq n a -> ViewR n a
viewr IsZero _ = EmptyR
viewr (IsSucc _) (NSeq s) = 
    case S.viewr s of
      S.EmptyR -> error "viewr: impossible" -- totality guaranteed
      xs S.:> x -> NSeq xs :> x


{- 
data ViewL (n :: Nat) a where
  EmptyL :: ViewL 0 a
  (:<) :: a -> NSeq n a -> ViewL (n+1) a
 
viewl :: forall a n. NSeq n a -> ViewL n a
viewl (NSeq proxy s) = 
  case testEquality (Proxy :: Proxy 0) proxy of 
    Just _ -> EmptyL 
    Nothing -> undefined -- case S.viewl s of
                         -- x S.:< xs -> x :< (NSeq Proxy xs)
-}

   