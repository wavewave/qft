{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Array
import           Data.Graph
import qualified Data.HashSet as H
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Proxy
-- import           Data.Sequence
import           Data.Tree
import           System.FilePath
import           System.Process
--
import           Data.Partition
import           Data.Permute
import           Data.SeqZipper
import           Data.Within
import           Diagram
import           Graph
import           McKay
import           Topology.Generate
import           Topology.PrettyPrint

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

  let g1 :: UndirGraph 4
      g1 = mkUndirGraph [a,d] 

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
  

  let testtree = createSearchTree asc 
      testtree2 = fmap isDiscrete testtree
      testtree3 = fmap discreteToPermutation testtree
      -- lst = (catMaybes . fmap discreteToPermutation . flatten) testtree
 
  putStrLn (drawTree (fmap show testtree))
  putStrLn (drawTree (fmap show testtree2))
  putStrLn (drawTree (fmap show testtree3))

  print (isomorphisms testtree)

  mapM_ print $ map (flip permuteGraph g2) (isomorphisms testtree)
  -- 
  putStrLn "finally" 
  let cg = canonicalLabel g2
  print cg
  
  --
  putStrLn "test globalVertexDegree"
  print (globalVertexDegree asc)
  -- 
  putStrLn "test undirToDirected"
  let dg = undirToDirected cg
  print dg 
  print (length (scc dg))
  
  -- 
  putStrLn "test vtype"
  let vtype1 = VK 1 "a" [(1,U,1)]
      -- vtype2 = VK 2 "b" [(1,U,2)]
      vtype3 = VK 3 "c" [(1,U,3)]
      -- vtype4 = VK 4 "d" [(1,U,4)]
      vtypes = H.fromList [vtype1,vtype3] -- [vtype1,vtype2,vtype3,vtype4]
 
  print (isCompatibleWith vtypes asc)

  
  let gg :: H.HashSet (UndirGraph 8)
      gg = H.singleton (mkUndirGraph [ ])
 
      -- generator = foldr H.insert H.empty . map canonicalLabel . concatMap generate1EdgeMore' . H.toList
      gg' = -- (generator . generator . generator . generator . generator . generator . generator . generator) gg 
            foldr1 (.) (replicate 8 nextEdgeLevelConnected) gg
       
  -- print (H.size gg')
  putStrLn "Test HERE"

  print (H.size gg')
  let -- asc' = mkAssocMap gg'
      resultgs =H.filter ((isCompatibleWith vtypes) . mkAssocMap) gg' 
  
  print (H.size resultgs)

  let myg = (head . H.toList) resultgs 
      myasc = mkAssocMap myg
  print (vertexCandidates vtypes myasc) 
  -- let namemap = M.fromList [ (1, "a" ),  (2, "b") , (3, "c") ]  



  -- print
  {-    
  let fnames = map (\x -> "test" ++ show x ++ ".dot") [1..]
      pairs= (zip fnames . map (makeDotGraph namemap) . H.toList) resultgs

  mapM_ (\(x,y) -> writeFile x y >> runNeato x) pairs

  writeFile "test.tex" $ makeTexFile (map (dropExtension.fst) pairs) 
 
  readProcess "pdflatex" [ "test.tex" ] ""

  return ()
  -}