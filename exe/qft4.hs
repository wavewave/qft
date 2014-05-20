{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Proxy
import           Permute


main :: IO ()
main = do  
  let tarr :: Array (Within 3) (Within 3)
      tarr = array (1, 3) [ (1, 2),(2,3),(3,1)]
      perm = case (mkPermutation tarr :: Either String (Permutation 3)) of
               Left str -> error str
               Right x -> x  
  print (permute perm 1)
