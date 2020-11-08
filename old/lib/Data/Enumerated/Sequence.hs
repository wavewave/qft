{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Enumerated.Sequence where

import GHC.TypeLits
--
import           Control.Applicative
import           Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import           Data.Monoid (Monoid)
import           Data.Singletons
import           Data.Traversable
import qualified Data.Sequence as S
--
import           Data.FromTuple
import           Data.PeanoNat

-- | 
newtype NSeq' (n :: PNat) a = NSeq (S.Seq a)


-- | to use type-level literal natural number
type NSeq (n :: Nat) = NSeq' (FromNat n) 

-- -- | smart constructor for NSeq 
-- mkNSeq :: 

instance FromTuple (NSeq' (PSucc (PSucc PZero)) Int) where
  type Tuple (NSeq' (PSucc (PSucc PZero)) Int) = (Int, Int) 
  fromTuple (a,b) = Right (NSeq (a S.<| b S.<| S.empty))

instance FromTuple (NSeq' (PSucc (PSucc (PSucc PZero))) Int) where
  type Tuple (NSeq' (PSucc (PSucc (PSucc PZero))) Int) = (Int, Int,Int) 
  fromTuple (a,b,c) = Right (NSeq (a S.<| b S.<| c S.<| S.empty))

instance FromTuple (NSeq' (PSucc (PSucc (PSucc (PSucc PZero)))) Int) where
  type Tuple (NSeq' (PSucc (PSucc (PSucc (PSucc PZero)))) Int) = (Int, Int, Int, Int) 
  fromTuple (a,b,c,d) = Right (NSeq (a S.<| b S.<| c S.<| d S.<| S.empty))

instance FromTuple (NSeq' (PSucc (PSucc (PSucc (PSucc (PSucc PZero))))) Int) where
  type Tuple (NSeq' (PSucc (PSucc (PSucc (PSucc (PSucc PZero))))) Int) = (Int, Int, Int, Int, Int) 
  fromTuple (a,b,c,d,e) = Right (NSeq (a S.<| b S.<| c S.<| d S.<| e S.<| S.empty))


deriving instance Functor (NSeq' n)

deriving instance F.Foldable (NSeq' n) 

deriving instance Applicative (NSeq' n) 

deriving instance Traversable (NSeq' n) 

deriving instance Alternative (NSeq' n)

deriving instance Monad (NSeq' n) 

deriving instance MonadPlus (NSeq' n) 

deriving instance Monoid (NSeq' n a)

deriving instance (Eq a) => Eq (NSeq' n a) 

deriving instance (Ord a) => Ord (NSeq' n a)

deriving instance (Read a) => Read (NSeq' n a)

deriving instance (Show a) => Show (NSeq' n a)

-- |
empty :: NSeq 0 a
empty = NSeq S.empty

-- |
singleton :: a -> NSeq 1 a
singleton x = NSeq (S.singleton x)

-- | left append
(<|) :: a -> NSeq' n a -> NSeq' (PSucc n) a
x <| NSeq xs = NSeq (x S.<| xs)
infixr 5 <|

-- | right append
(|>) :: forall n a. NSeq' n a -> a -> NSeq' (PSucc n) a
NSeq xs |> x = NSeq (xs S.|> x)
infixl 5 |>

-- | concat
(><) :: forall m n a. NSeq' m a -> NSeq' n a -> NSeq' (m :+: n) a
NSeq xs >< NSeq ys = NSeq (xs S.>< ys)
infixr 5 ><

-- | left viewer
data ViewL' (n :: PNat) a where
  EmptyL :: ViewL' PZero a
  (:<) :: a -> NSeq' n a -> ViewL' (PSucc n) a
 
-- | left view function
viewl :: Sing n -> NSeq' n a -> ViewL' n a
viewl PSZero _  = EmptyL
viewl (PSSucc _) (NSeq s) =  
    case S.viewl s of 
      S.EmptyL -> error "viewl: impossible" -- totality guaranteed. 
      x S.:< xs -> x :< NSeq xs

-- | right viewer
data ViewR' (n :: PNat) a where
  EmptyR :: ViewR' PZero a
  (:>) :: NSeq' n a -> a -> ViewR' (PSucc n) a

-- | right view function
viewr :: Sing n -> NSeq' n a -> ViewR' n a
viewr PSZero _ = EmptyR
viewr (PSSucc _) (NSeq s) = 
    case S.viewr s of
      S.EmptyR -> error "viewr: impossible" -- totality guaranteed
      xs S.:> x -> NSeq xs :> x
