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
