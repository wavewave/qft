module Main where

import qualified Data.Foldable as F (forM_) 
import Topology 

a = mkUndirEdge 1 2 

b = mkUndirEdge 1 3

c = mkUndirEdge 2 3

d = mkUndirEdge 1 4

elst = mkSortedEdges [a,b,c,d]
 
vlst = mkSortedVertices [1, 2, 3]

vlst' = mkSortedVertices [1,4,7,11,22] 

main :: IO ()
main = do 
  putStrLn "Topology test"
  print a
  print elst
  print vlst
  let mg1 = mkUndirGraph [a,b,c] [1,2,3]
  F.forM_ mg1 $ \g1 -> do
    print g1 
    print (g1 `addVertex` 4)
    print (g1 `addVertex` 1)
    print (g1 `addEdge` a )
    print (g1 `addEdge` d )
    print (g1 `addVertex` 4 `addEdge` d )

    print (pick2 vlst)

    putStrLn "-----------"
    print (pick2 vlst')
    print (pick2distinct vlst')
    putStrLn "==========="
    mapM_ print (generate1EdgeMore g1)