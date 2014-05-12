module Topology.PrettyPrint where

import Data.List (intercalate)
import Topology 

makeDotEdge :: UndirEdge -> String
makeDotEdge e = show (edgeV1 e) ++ " -- " ++ show (edgeV2 e) ++ ";"

makeDotVertex :: Vertex -> String 
makeDotVertex v = show v ++ " ;"

makeDotGraph :: UndirGraph -> String 
makeDotGraph gr = let header = "graph {"
                      st1 = (map makeDotVertex . unSV . vertices) gr
                      st2 = (map makeDotEdge . unSE . edges ) gr
                      footer = "}" 
                  in intercalate "\n" ([header] ++ st1 ++ st2 ++ [footer])
