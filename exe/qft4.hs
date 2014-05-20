{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Proxy
import           Unsafe.Coerce
import           Permute


main :: IO ()
main = do 
  print $ listFromInterval (mkNatInterval :: NatInterval 1 3)

  let i1 = mkNatInterval :: NatInterval 2 9
      i2 = regularizeInterval i1 
  print $ listFromInterval i1
  print $ listFromInterval i2

  let ra = mkRegArray (11 `Cons` (12 `Cons` (13 `Cons` (14 `Cons` (15 `Cons` Nil)))))
      two = NumberInInterval (Proxy :: Proxy 2) (mkNatInterval :: NatInterval 1 5)
  print (ra ^! two )
