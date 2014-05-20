module Graph where

import Control.Applicative
import Control.Monad
import Data.Function (on)
import Data.List (nub, sort, sortBy, tails, lookup,group )
import Data.Maybe (mapMaybe, fromJust)
-- import qualified Data.Permute as P
-- 
import Permute
-- 
import Debug.Trace
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

