{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Proxy
import           Permute


main :: IO ()
main = do 
  {-
  print $ listFromInterval (mkNatInterval :: NatInterval 1 3)

  let i1 = mkNatInterval :: NatInterval 2 9
      i2 = regularizeInterval i1 
  print $ listFromInterval i1
  print $ listFromInterval i2
  -}
 
  let tarr :: Array (Within 3) (Within 3)
      tarr = array (1, 3) [ (1, 2),(2,3),(3,1)]
      perm = case (mkPermutation tarr :: Either String (Permutation 3)) of
               Left str -> error str
               Right x -> x  
  print (permute perm 1)

  {-     
  (print . backwardArray) rarr

  let p = case (mkPermutation rarr :: Either String (Permutation 3)) of
            Left str -> error str
            Right x -> x 
      -- intval = mkNatInterval :: RegInterval 3
  
  print (permuteBackward p (XInInterval (Proxy :: Proxy 3)))
  

  -- let b = isElemType (Proxy :: Proxy 1) intval :: Bool

  -- print b

  -}