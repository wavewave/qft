module Topology.PrettyPrint where

import GHC.TypeLits
-- 
import Data.Array ((!))
import Data.List (intercalate)
import qualified Data.Map as M
import System.FilePath ( (<.>), dropExtension )
import System.Process
--
import Data.Fin1
import Graph

type NameMap n = M.Map (Fin1 n) String

{- 
testfun :: Int -> String
testfun 1 = "blue"
testfun 2 = "yellow"
testfun 3 = "red"
testfun _ = "black"
-}

makeDotEdge :: UndirEdge n -> String
makeDotEdge e = show (edgeV1 e) ++ " -- " ++ show (edgeV2 e) ++ ";"

makeDotVertex :: NameMap n -> AssocMap n -> Vertex n -> String 
makeDotVertex nm asc v = let _lv = length (asc ! v);
                             mname = M.lookup v nm 
                         in  show v ++ maybe "" (\name -> " [ label = " ++ name ++ " ] ;") mname

makeDotGraph :: (KnownNat n) => NameMap n -> UndirGraph n -> String 
makeDotGraph nm gr = let header = "graph {\nsplines=true; esep=1;"
                         str_v = map (makeDotVertex nm (mkAssocMap gr)) interval
                         str_e = (map makeDotEdge . unSE . edges ) gr
                         footer = "}" 
                     in intercalate "\n" ([header] ++ str_v ++ str_e ++ [footer])

runDot :: FilePath -> IO ()
runDot f = readProcess "dot" [ "-Tpdf", f, "-o", dropExtension f <.> "pdf" ] ""
           >> return ()

runNeato :: FilePath -> IO ()
runNeato f = readProcess "neato" [ "-Tpdf", f, "-o", dropExtension f <.> "pdf" ] ""
           >> return ()

makeTexFile :: [FilePath] -> String
makeTexFile filenames = 
    header ++ (intercalate "\n" .  map makeTexFig) filenames  ++ footer
  where 
    header = "\\documentclass{revtex4}\n\\usepackage{graphicx}\n\\begin{document}\n"
    footer = "\n\\end{document}\n"   

makeTexFig :: FilePath -> String
makeTexFig filename = "\\includegraphics[width=5cm]{" ++ filename ++ ".pdf}"