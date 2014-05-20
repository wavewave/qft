{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Proxy
-- import           Unsafe.Coerce
import           Permute


main :: IO ()
main = do 
  print $ listFromInterval (mkNatInterval :: NatInterval 1 3)

  let i1 = mkNatInterval :: NatInterval 2 9
      i2 = regularizeInterval i1 
  print $ listFromInterval i1
  print $ listFromInterval i2

{-
  let ra = mkRegArray (11 `Cons` (12 `Cons` (13 `Cons` (14 `Cons` (15 `Cons` Nil)))))
      two = NumberInInterval (Proxy :: Proxy 2) (mkNatInterval :: NatInterval 1 5)
  print (ra ^! two )
-}


  let tarr = array (1,3) [(1,2),(2,3),(3,1)]
      rarr = case (mkRevArray tarr :: Either String RevArray) of
               Left str -> error str
               Right x -> x 
     
  (print . backwardArray) rarr

  let p = case (mkPermutation rarr :: Either String (Permutation 4)) of
            Left str -> error str
            Right x -> x 
      intval = mkNatInterval :: RegInterval 4
  
  print (permuteForward p (NumberInInterval (Proxy :: Proxy 1) intval))
  