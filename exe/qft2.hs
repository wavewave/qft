module Main where

import qualified Data.Foldable as F (forM_) 
import System.FilePath
--
import Topology 
import Topology.PrettyPrint

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
  let mg0 = mkUndirGraph [] [1,2,3,4]
  F.forM_ mg0 $ \g0 -> do
    putStrLn "okay"
    let gs = do 
           g1 <- generate1EdgeMore g0
           g2 <- generate1EdgeMore g1
           return g2

    let fnames = map (\x -> "test" ++ show x ++ ".dot") [1..]
        pairs= zip fnames (map makeDotGraph gs)

    mapM_ (\(x,y) -> writeFile x y >> runDot x) pairs

    writeFile "test.tex" $ makeTexFile (map (dropExtension.fst) pairs) 