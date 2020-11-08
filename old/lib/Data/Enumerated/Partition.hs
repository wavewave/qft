{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Enumerated.Partition where

import Data.Enumerated.Sequence
-- import Data.FromTuple
import Data.PeanoNat
-- import Data.Promotion.Prelude.Base

data Vector (n :: PNat) a where
  Empty :: Vector PZero a
  VCons :: a -> Vector n a -> Vector (PSucc n) a

type family Sum (ns :: [PNat]) where
  Sum '[] = PZero
  Sum (n ': ns) = n :+: Sum ns

test :: (Sum '[ FromNat 0 , FromNat 0, FromNat 0 ] ~ FromNat 0) => Int
test = 3
-- type Sum = Foldr (:+:) PZero

-- chop :: Proxy n -> Vector m a -> (n ,Vector (m-n) a)
-- chop = undefined 


data HList (k :: [*]) where
  Nil :: HList '[] 
  Cons :: a -> HList as -> HList (a ': as)

infixr 5 `Cons`

data Partition (k :: [(PNat,*)]) where
  PNil :: Partition '[] 
  PCons :: NSeq' (PSucc n) a -> Partition ns -> Partition ('( PSucc n, NSeq' (PSucc n) a ) ': ns) 

infixr 5 `PCons`

instance Show (HList '[]) where
  show _ = "Nil"

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  show (x `Cons` xs) = show x ++ " : " ++ show xs 


instance Show (Partition '[]) where
  show _ = "PNil"

instance (Show a, Show (Partition ns)) => Show (Partition ('(n,a) ': ns)) where
  show (x `PCons` xs) = show x ++ " : " ++ show xs 


{-
import GHC.TypeLits
-- 
import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (left, runEitherT)
import           Data.Array (listArray)
import           Data.Array.ST
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Sequence (Seq, ViewL(..), viewl, fromList, singleton)
-- 
import           Data.Permute
import           Data.SeqZipper
import           Data.Fin1

-- |
newtype OrderedPartition n = OP { getPartition :: Seq [Fin1 n] }
                           deriving Show

mkOrderedPartition :: forall (n :: Nat) . (KnownNat n) => [ [ Fin1 n ] ] -> Either String (OrderedPartition n)
mkOrderedPartition lst = runST action
  where nn = order 
        action :: forall s. ST s (Either String (OrderedPartition n))        
        action =   runEitherT $ do 
                     rarr <- lift (newArray (1,nn) Nothing :: ST s (STArray s (Fin1 n) (Maybe ())))
                     F.forM_ (concat lst) $ \r -> do
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not a partition"
                         Nothing -> lift (writeArray rarr r (Just ()))
                     F.forM_ [1..nn] $ \r -> 
                       maybe (left "not a partition") (const (return ())) =<< lift (readArray rarr r)
                     (return . OP . fromList ) lst


unitPartition :: forall n. (KnownNat n) => OrderedPartition n
unitPartition = OP (singleton interval)

firstNontrivial :: OrderedPartition n -> Maybe (SeqZipper [Fin1 n])
firstNontrivial = listToMaybe . dropWhile ( ( == 1) . length . current ) . zippers



zippers :: OrderedPartition n -> [ SeqZipper [Fin1 n] ]
zippers (OP ptn) = (catMaybes . takeWhile (isJust) . iterate (moveRight =<<)) (pure ptn1)
  where ptn1 = case viewl ptn of
                 EmptyL -> error "impossble" -- guaranteed from OrderedPartition and n >= 1
                 x :< xs -> fromNonEmptySeq (x,xs)

locateInPartition :: OrderedPartition n -> Fin1 n -> SeqZipper [Fin1 n]
locateInPartition ptn x = head (filter (p x) (zippers ptn))   -- this is guaranteed for ordered partition
  where p y z = y `elem` current z

isDiscrete :: OrderedPartition n -> Bool 
isDiscrete = all ((== 1) . length) . F.toList . getPartition

discreteToPerm :: (KnownNat n) => OrderedPartition n -> Maybe (Perm n)
discreteToPerm ptn = if isDiscrete ptn 
                            then case (mkPerm . listArray (1,order) . concat . F.toList . getPartition) ptn of 
                                   Left err -> error err -- cannot happen. guaranteed by OrderedPartition
                                   Right p -> Just p
                            else Nothing
-}