{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Proxy
--
import           Graph
import           Permute

a :: UndirEdge 4
a = mkUndirEdge 1 2 

b :: UndirEdge 4
b = mkUndirEdge 1 3

c :: UndirEdge 4
c = mkUndirEdge 2 3

d :: UndirEdge 4
d = mkUndirEdge 1 4


main :: IO ()
main = do  
  let tarr :: Array (Within 4) (Within 4)
      tarr = array (1, 4) [ (1, 4),(2,3),(3,1), (4,2)]
      perm = case (mkPermutation tarr :: Either String (Permutation 4)) of
               Left str -> error str
               Right x -> x  
  print (permute perm 1)
  print (invperm perm 2)

  print (map (permute perm) [ 1,2,3,4 ] )


  let mg1 :: Maybe (UndirGraph 4)
      mg1 = mkUndirGraph [a,d] [1,2,3,4]

  case mg1 of 
    Nothing -> return ()
    Just g1 -> do 
      print g1
      print (permuteGraph perm g1) 
