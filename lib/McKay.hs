module McKay where

import qualified Data.Map as M
-- 
import Graph 
-- import Permute

shatteringBy :: AssocMap n -> [Vertex n] -> [Vertex n] -> [ [Vertex n] ] 
shatteringBy arr vi vj = let resultmap = foldr f M.empty vj 
                             asclst = M.toAscList resultmap 
                         in map ((flip ($) []) . snd) asclst
  where  
    f x acc = let d = degree arr vi x
              in M.insertWith (\n o ->  n . o)  d (x:) acc 
        
