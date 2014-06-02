{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
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


module Data.EnumeratedSequence where

import GHC.TypeLits
--
import Control.Applicative
import Control.Monad (MonadPlus)
import qualified Data.Foldable as F
import Data.Monoid (Monoid)
import           Data.Singletons
import Data.Traversable
import qualified Data.Sequence as S
-- import           Data.Proxy
-- import Data.Typeable (Typeable)
import Data.Type.Equality



data MNat = MZero | MSucc MNat

type family n1 :+: n2 where
  MZero :+: n2 = n2
  (MSucc n1') :+: n2 = MSucc (n1' :+: n2)

data instance Sing (a :: MNat) where
  MyZero :: Sing MZero
  MySucc :: Sing n -> Sing (MSucc n)

type family FromNat (n :: Nat) where
  FromNat 0 = MZero
  FromNat n = MSucc (FromNat (n-1))



plus_id_r :: forall (n :: MNat) . Sing n -> ((n :+: MZero) :~: n)
plus_id_r MyZero = Refl
plus_id_r (MySucc n) = gcastWith (plus_id_r n) Refl  

plus_succ_r :: forall (n1 :: MNat) (n2 :: MNat) . Sing n1 -> Sing n2 -> (n1 :+: (MSucc n2)) :~: (MSucc (n1 :+: n2))
plus_succ_r MyZero _  = Refl
plus_succ_r (MySucc n1) n2 = gcastWith (plus_succ_r n1 n2) Refl




newtype NSeq' (n :: MNat) a = NSeq (S.Seq a)

type NSeq (n :: Nat) = NSeq' (FromNat n) 


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

deriving instance Functor (NSeq' n)

deriving instance F.Foldable (NSeq' n) 

deriving instance Applicative (NSeq' n) 

deriving instance Traversable (NSeq' n) 

deriving instance Alternative (NSeq' n)

deriving instance Monad (NSeq' n) 

deriving instance MonadPlus (NSeq' n) 

deriving instance Monoid (NSeq' n a)

-- deriving instance Typeable (NSeq n)

deriving instance (Eq a) => Eq (NSeq' n a) 

-- deriving instance (Data a) => Data (NSeq n a)

deriving instance (Ord a) => Ord (NSeq' n a)

deriving instance (Read a) => Read (NSeq' n a)

deriving instance (Show a) => Show (NSeq' n a)



empty :: NSeq 0 a
empty = NSeq S.empty

singleton :: a -> NSeq 1 a
singleton x = NSeq (S.singleton x)

(<|) :: a -> NSeq' n a -> NSeq' (MSucc n) a
x <| NSeq xs = NSeq (x S.<| xs)

infixr 5 ><
infixr 5 <|
infixl 5 |>

(|>) :: forall n a. NSeq' n a -> a -> NSeq' (MSucc n) a
NSeq xs |> x = NSeq (xs S.|> x)

(><) :: forall m n a. NSeq' m a -> NSeq' n a -> NSeq' (m :+: n) a
NSeq xs >< NSeq ys = NSeq (xs S.>< ys)

data ViewL' (n :: MNat) a where
  EmptyL :: ViewL' MZero a
  (:<) :: a -> NSeq' n a -> ViewL' (MSucc n) a
 
viewl :: Sing n -> NSeq' n a -> ViewL' n a
viewl MyZero _  = EmptyL
viewl (MySucc _) (NSeq s) =  
    case S.viewl s of 
      S.EmptyL -> error "viewl: impossible" -- totality guaranteed. 
      x S.:< xs -> x :< NSeq xs

data ViewR' (n :: MNat) a where
  EmptyR :: ViewR' MZero a
  (:>) :: NSeq' n a -> a -> ViewR' (MSucc n) a

viewr :: Sing n -> NSeq' n a -> ViewR' n a
viewr MyZero _ = EmptyR
viewr (MySucc _) (NSeq s) = 
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

   