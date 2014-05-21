module Topology.PrettyPrint where

import Data.Array
import Data.Graph
import Data.List (intercalate)
import System.FilePath ( (<.>), dropExtension )
import System.Process
--

-- |
makeDotEdge :: (Vertex,Vertex) -> String
makeDotEdge (v1,v2) = show v1 ++ " -- " ++ show v2 ++ ";"

-- |
makeDotVertex :: Vertex -> String 
makeDotVertex v = show v ++ " ;"

-- |
makeDotGraph :: Graph -> String 
makeDotGraph gr = let header = "graph {"
                      is = indices gr 
                      pairs = [ (i,j) | i <- is, j <- gr ! i ]
                      str = map  makeDotEdge pairs
                      footer = "}" 
                  in intercalate "\n" ([header] ++ str ++ [footer])

-- |
runDot :: FilePath -> IO ()
runDot f = readProcess "dot" [ "-Tpdf", f, "-o", dropExtension f <.> "pdf" ] ""
           >> return ()

-- |
makeTexFile :: [FilePath] -> String
makeTexFile filenames = 
    header ++ (intercalate "\n" .  map makeTexFig) filenames  ++ footer
  where 
    header = "\\documentclass{revtex4}\n\\usepackage{graphicx}\n\\begin{document}\n"
    footer = "\n\\end{document}\n"   

-- |
makeTexFig :: FilePath -> String
makeTexFig filename = "\\includegraphics[width=5cm]{" ++ filename ++ ".pdf}"
