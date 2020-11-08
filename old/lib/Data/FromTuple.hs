{-# LANGUAGE TypeFamilies #-}

module Data.FromTuple where

class FromTuple a where 
  type Tuple a :: * 
  fromTuple :: Tuple a -> Either String a
  -- toTuple :: a -> Tuple a 
