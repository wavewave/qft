module OldTopology.PrettyPrint where

import Data.List (intercalate)
import System.FilePath ( (<.>), dropExtension )
import System.Process
--
-- import OldTopology 
import Graph


makeDotEdge :: UndirEdge n -> String
makeDotEdge e = show (edgeV1 e) ++ " -- " ++ show (edgeV2 e) ++ ";"

makeDotVertex :: Vertex n -> String 
makeDotVertex v = show v ++ " ;"

makeDotGraph :: UndirGraph n -> String 
makeDotGraph gr = let header = "graph {"
                      st = (map makeDotEdge . unSE . edges ) gr
                      footer = "}" 
                  in intercalate "\n" ([header] ++ st ++ [footer])

runDot :: FilePath -> IO ()
runDot f = readProcess "dot" [ "-Tpdf", f, "-o", dropExtension f <.> "pdf" ] ""
           >> return ()

makeTexFile :: [FilePath] -> String
makeTexFile filenames = 
    header ++ (intercalate "\n" .  map makeTexFig) filenames  ++ footer
  where 
    header = "\\documentclass{revtex4}\n\\usepackage{graphicx}\n\\begin{document}\n"
    footer = "\n\\end{document}\n"   

makeTexFig :: FilePath -> String
makeTexFig filename = "\\includegraphics[width=5cm]{" ++ filename ++ ".pdf}"