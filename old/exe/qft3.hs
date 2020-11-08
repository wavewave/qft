module Main where

import           Control.Applicative
import           Data.Array
import qualified Data.Foldable as F (forM_) 
import           Data.Function (on)
import           Data.Graph.Automorphism
import           Data.Graph.Construction
import           Data.List (nubBy, sortBy)
import           Data.Maybe (fromJust)
import           System.FilePath
-- 
import           Topology.PrettyPrint

testgraph = undirG (cycleG 3)
-- withUnitPartition canonicGraph (prismG 3) -- productG arcG vertexG

main :: IO ()
main = do 
  putStrLn "HGAL test"
  print testgraph
  let gs' = [testgraph]
  let fnames = map (\x -> "test" ++ show x ++ ".dot") [1..]
      pairs= zip fnames (map makeDotGraph gs')

  mapM_ (\(x,y) -> writeFile x y >> runDot x) pairs

  writeFile "test.tex" $ makeTexFile (map (dropExtension.fst) pairs) 
