{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Partition where

import GHC.TypeLits
-- 
import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (left, runEitherT)
import           Data.Array.ST
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence (Seq, ViewL(..), viewl, fromList, singleton)
-- 
import           Data.SeqZipper
import           Data.Within

-- |
newtype OrderedPartition n = OP { getPartition :: Seq [Within n] }
                           deriving Show

mkOrderedPartition :: forall (n :: Nat) . (KnownNat n) => [ [ Within n ] ] -> Either String (OrderedPartition n)
mkOrderedPartition lst = runST action
  where nn = order (Proxy :: Proxy n)
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
                     (return . OP . fromList ) lst


unitPartition :: forall n. (KnownNat n) => OrderedPartition n
unitPartition = OP (singleton (interval (Proxy :: Proxy n)))

zippers :: OrderedPartition n -> [ SeqZipper [Within n] ]
zippers (OP ptn) = (catMaybes . takeWhile (isJust) . iterate (moveRight =<<)) (pure ptn1)
  where ptn1 = case viewl ptn of
                 EmptyL -> error "impossble" -- guaranteed from OrderedPartition and n >= 1
                 x :< xs -> fromNonEmptySeq (x,xs)

locateInPartition :: OrderedPartition n -> Within n -> SeqZipper [Within n]
locateInPartition ptn x = head (filter (p x) (zippers ptn))   -- this is guaranteed for ordered partition
  where p y z = y `elem` current z

