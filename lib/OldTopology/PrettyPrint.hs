module OldTopology.PrettyPrint where

import Data.List (intercalate)
import System.FilePath ( (<.>), dropExtension )
import System.Process
--
import OldTopology 


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

runDot :: FilePath -> IO ()
runDot f = readProcess "dot" [ "-Tpdf", f, "-o", dropExtension f <.> "pdf" ] ""
           >> return ()

makeTexFile filenames = 
    header ++ (intercalate "\n" .  map makeTexFig) filenames  ++ footer
  where 
    header = "\\documentclass{revtex4}\n\\usepackage{graphicx}\n\\begin{document}\n"
    footer = "\n\\end{document}\n"   

makeTexFig filename = "\\includegraphics[width=5cm]{" ++ filename ++ ".pdf}"