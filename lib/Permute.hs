{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Permute where

-- import Data.Type.Equality
import GHC.TypeLits
import Data.Proxy

-- data Proxy (a :: Nat) = Proxy 

data TCMP = TLT | TEQ | TGT

data Compare (a :: Nat) (b :: Nat) (r :: TCMP) where
  LessThan    :: forall a b x . (a+x) ~ b => Proxy a -> Proxy b -> Compare a b TLT
  Equal       :: forall a b   . a ~ b     => Proxy a -> Proxy b -> Compare a b TEQ
  GreaterThan :: forall a b x . a ~ (b+x) => Proxy a -> Proxy b -> Compare a b TGT

test :: Compare a b r -> String 
test (LessThan _ _)     = "LessThan"
test (Equal _ _)        = "Equal"
test (GreaterThan _ _)  = "GreaterThan"


fun :: IO ()
fun = do 
  print "hello"
  print (test (LessThan (Proxy :: Proxy 1) (Proxy :: Proxy 3)))
  print (test (Equal (Proxy :: Proxy 1) (Proxy :: Proxy 1)))
