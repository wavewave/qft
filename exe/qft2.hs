{-# LANGUAGE DataKinds #-}

module Main where

-- import           Control.Applicative
import qualified Data.Foldable as F (forM_) 
import           Data.Function (on)
import           Data.HashSet (insert,empty,size,toList)
import           Data.List (nubBy, sortBy)
import           Data.Maybe (fromJust)
import qualified Data.Permute as P
import           System.FilePath
-- 
import           Graph
import           McKay
-- import           OldTopology 
import           OldTopology.PrettyPrint
import           Topology.Generate


main :: IO ()
main = do 
  putStrLn "Topology test"
  let g0 :: UndirGraph 9
      g0 = mkUndirGraph [ mkUndirEdge 1 2 
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

  let g1 :: UndirGraph 4
      g1 = mkUndirGraph [ mkUndirEdge 1 2
                        , mkUndirEdge 2 3
                        , mkUndirEdge 3 4
                        , mkUndirEdge 4 1]

  let gs = (map canonicalLabel . generate1EdgeMore) g1
      gs' = foldr insert empty gs 
      gs'' = (foldr insert empty . map canonicalLabel . concatMap generate1EdgeMore . toList) gs'    
  -- print (length gs'')
  -- print (size gs')


  let fnames = map (\x -> "test" ++ show x ++ ".dot") [1..]
      pairs= (zip fnames . map makeDotGraph . toList) gs''

  mapM_ (\(x,y) -> writeFile x y >> runDot x) pairs

  writeFile "test.tex" $ makeTexFile (map (dropExtension.fst) pairs) 

      
{-  
  -- mapM_ print gs
  -- maybe (return ()) (\g' -> mapM_ print g') (mapM canonicalize gs)

-}    