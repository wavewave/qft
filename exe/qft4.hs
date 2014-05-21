{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Sequence
--
import           Graph
import           McKay
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
  print (permute (inverse perm) 2)

  print (map (permute perm) [ 1,2,3,4 ] )


  let g1 :: UndirGraph 4
      g1 = mkUndirGraph [a,d] 

  print g1
  print (permuteGraph perm g1) 
  let asc = mkAssocMap g1
  print (map (degree asc [1,2]) [1,2,3,4])

  let eop = mkOrderedPartition [ [ 1,2,3] , [4] , [5,6] ] :: Either String (OrderedPartition 6)
  print eop


  let g2 :: UndirGraph 9
      g2 = mkUndirGraph [ mkUndirEdge 1 2 
                        , mkUndirEdge 2 3
                        , mkUndirEdge 3 6
                        , mkUndirEdge 6 9
                        , mkUndirEdge 9 8
                        , mkUndirEdge 8 7 
                        , mkUndirEdge 7 4
                        , mkUndirEdge 4 1
                        , mkUndirEdge 2 5
                        , mkUndirEdge 5 8
                        , mkUndirEdge 4 5
                        , mkUndirEdge 5 6 ]

  print  g2
  let asc = mkAssocMap g2
  print (shatteringBy asc [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9])