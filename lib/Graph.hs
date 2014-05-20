module Graph where

import Data.List (nub, sort, sortBy )
-- 
import Permute
-- 
import Prelude hiding (lookup)


type Vertex n = Within n

data UndirEdge n = UndirEdge { edgeV1 :: Within n
                             , edgeV2 :: Within n }
                 deriving (Show, Eq)

verticesFromEdge :: UndirEdge n -> [Within n]
verticesFromEdge (UndirEdge x y) = [x,y]

isSelfish :: UndirEdge n -> Bool
isSelfish (UndirEdge x y) = x == y 
 
edgecmp :: UndirEdge n -> UndirEdge n -> Ordering
edgecmp (UndirEdge x1 x2) (UndirEdge y1 y2) = compare (x1,x2) (y1,y2)  

mkUndirEdge :: Within n -> Within n -> UndirEdge n 
mkUndirEdge x y | x <= y = UndirEdge x y 
                | otherwise = UndirEdge y x 

data SortedVertices n = SV { unSV :: [Vertex n] }
                      deriving (Show, Eq)

mkSortedVertices :: [Vertex n] -> SortedVertices n
mkSortedVertices = SV . nub . sort 

data SortedEdges n = SE { unSE :: [UndirEdge n] }
                   deriving (Show, Eq)

mkSortedEdges :: [UndirEdge n] -> SortedEdges n
mkSortedEdges xs = SE (sortBy edgecmp xs) 

data UndirGraph n = UG { edges :: SortedEdges n
                       , vertices :: SortedVertices n
                       }
                deriving (Show, Eq)

mkUndirGraph :: [UndirEdge n] -> [Vertex n] -> Maybe (UndirGraph n)
mkUndirGraph es vs = 
  let v1s = (concatMap verticesFromEdge) es
      b = all (`elem` vs) v1s 
  in if b then Just (UG (mkSortedEdges es) (mkSortedVertices vs)) else Nothing


permuteEdge :: Permutation n -> UndirEdge n -> UndirEdge n
permuteEdge p (UndirEdge v1 v2) = mkUndirEdge (permute p v1) (permute p v2)

permuteGraph :: Permutation n -> UndirGraph n -> UndirGraph n 
permuteGraph p (UG (SE es) vs) = UG (mkSortedEdges (map (permuteEdge p) es)) vs