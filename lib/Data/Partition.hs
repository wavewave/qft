{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Partition where

import GHC.TypeLits
-- 
import           Control.Monad.ST
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (left, runEitherT)
import           Data.Array.ST
import qualified Data.Foldable as F
import           Data.Proxy
import           Data.Sequence (Seq, fromList, singleton)
-- 
import           Data.Within

-- |
newtype OrderedPartition n = OP { getPartition :: Seq [Within n] }
                           deriving Show

mkOrderedPartition :: forall (n :: Nat) . (KnownNat n) => [ [ Within n ] ] -> Either String (OrderedPartition n)
mkOrderedPartition lst = runST action
  where nn = MkWithin (natVal (Proxy :: Proxy n))
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