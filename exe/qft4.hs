{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Sequence
import           Data.Tree
--
import           Data.Partition
import           Data.Permute
import           Data.SeqZipper
import           Data.Within
import           Graph
import           McKay

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
  -- print (shatteringBy asc [1,2,3,4,5,6,7,8,9] [1,2,3,4,5,6,7,8,9])

  putStrLn "Shattering test"
  let test1 = shatter asc unitPartition
  print test1
  case test1 of
    test11:_ -> print (shatter asc test11)
    _ -> print "nothing"

  print (shatteringBy asc [1,3,7,9] [1,3,7,9])


  putStrLn "Equitable refinement test"
  let e1 = equitableRefinement asc unitPartition
  print e1

  print (equitableRefinement asc e1)

  print (unSZ (locateInPartition e1 8))

  let r2 = splittingBy e1 1
  print r2
  putStrLn "next level"
  -- mapM_ (print . unSZ) (zippers r2)
  -- print (shatteringBy asc [2,4,6,8] [1])
  let e2 = equitableRefinement asc r2
  -- mapM_ print (shatter asc r2)
  print e2
  print (fmap unSZ (firstNontrivial e2))

  
  let r3 = splittingBy e2 3
      e3 = equitableRefinement asc r3
  print e3 
  print (fmap unSZ (firstNontrivial e3))

  

  -- putStrLn "new test"
  -- print ( shatteringBy asc [3,7,9] [6,8] )
{-
  let eop = mkOrderedPartition [ [ 1,2,3] , [4] , [5,6] ] :: Either String (OrderedPartition 6)
  case eop of 
    Left err -> error err
    Right op -> do
      print opt
      mapM_ print (shatter undefined op)
-}

  

  let testtree = createSearchTree asc 
      testtree2 = fmap isDiscrete testtree
      testtree3 = fmap discreteToPermutation testtree
      -- lst = (catMaybes . fmap discreteToPermutation . flatten) testtree
 
  putStrLn (drawTree (fmap show testtree))
  putStrLn (drawTree (fmap show testtree2))
  putStrLn (drawTree (fmap show testtree3))

  print (isomorphisms testtree)

  