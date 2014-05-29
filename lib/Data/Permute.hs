{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Permute where

import GHC.TypeLits
-- 
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (runEitherT, left, hoistEither)
import Data.Array
import Data.Array.ST 
import qualified Data.Foldable as F
-- 
import Data.Fin1
import Util
        
-- |
data Permutation (n :: Nat) = Permutation { forward :: Array (Fin1 n) (Fin1 n)
                                          , backward :: Array (Fin1 n)  (Fin1 n) }
                            deriving (Show)

-- |
mkPermutation :: forall (n :: Nat) . (KnownNat n) => Array (Fin1 n) (Fin1 n) -> Either String (Permutation n)
mkPermutation arr= runST action
  where action :: forall s. ST s (Either String (Permutation n))        
        action =   runEitherT $ do 
                     let (i1,i2) = bounds arr
                     hoistEither (guardEither "i1 is not 1" (i1 == 1))
                     rarr <- lift (newArray (i1,i2) Nothing :: ST s (STArray s (Fin1 n) (Maybe (Fin1 n))))
                     rarr' <- lift (newArray_ (i1,i2) :: ST s (STArray s (Fin1 n) (Fin1 n)))
                     F.forM_ [i1..i2] $ \i -> do
                       let r = arr ! i
                       o <- lift (readArray rarr r)
                       case o of
                         Just _ -> left "not reversible"
                         Nothing -> lift (writeArray rarr r (Just i))
                     F.forM_ [i1..i2] $ \r -> 
                       maybe (left "not reversible") (\i -> lift (writeArray rarr' r i)) =<< lift (readArray rarr r)
                     return . Permutation arr =<< lift (freeze rarr')

 
-- |
permute :: Permutation n -> Fin1 n -> Fin1 n
permute p i = forward p ! i

-- | 
inverse :: Permutation n -> Permutation n 
inverse (Permutation f b) = Permutation b f

