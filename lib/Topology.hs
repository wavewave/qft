{-# LANGUAGE TupleSections #-}

module Topology
( Vertex
, UndirEdge(edgeV1, edgeV2)
, UndirGraph(vertices, edges)
, SortedVertices(unSV)
, SortedEdges(unSE)
, edgecmp
, mkUndirEdge
, mkSortedVertices
, mkSortedEdges
, mkUndirGraph
, addVertex
, addEdge
, pick2distinct
, pick2
, generate1EdgeMore
) where

import Data.List (nub, sort, sortBy, tails )
import Data.Maybe (mapMaybe)

type Vertex = Int

data UndirEdge = UndirEdge { edgeV1 :: Int
                           , edgeV2 :: Int }
               deriving Show

verticesFromEdge :: UndirEdge -> [Int]
verticesFromEdge (UndirEdge x y) = [x,y]

isSelfish :: UndirEdge -> Bool
isSelfish (UndirEdge x y) = x == y 

edgecmp :: UndirEdge -> UndirEdge -> Ordering
edgecmp (UndirEdge x1 x2) (UndirEdge y1 y2) = compare (x1,x2) (y1,y2)  

mkUndirEdge :: Int -> Int -> UndirEdge 
mkUndirEdge x y | x <= y = UndirEdge x y 
                | otherwise = UndirEdge y x 

data SortedVertices = SV { unSV :: [Vertex] }
                    deriving Show

mkSortedVertices :: [Int] -> SortedVertices 
mkSortedVertices = SV . nub . sort 

data SortedEdges = SE { unSE :: [UndirEdge] }
                 deriving Show

mkSortedEdges :: [UndirEdge] -> SortedEdges
mkSortedEdges xs = SE (sortBy edgecmp xs) 

data UndirGraph = UG { edges :: SortedEdges
                     , vertices :: SortedVertices
                     }
                deriving Show

mkUndirGraph :: [UndirEdge] -> [Vertex] -> Maybe UndirGraph
mkUndirGraph es vs = 
  let v1s = (concatMap verticesFromEdge) es
      b = all (`elem` vs) v1s 
  in if b then Just (UG (mkSortedEdges es) (mkSortedVertices vs)) else Nothing


addVertex :: UndirGraph -> Vertex -> UndirGraph
addVertex (UG (SE es) (SV vs)) v = let vs' = v : vs in UG (SE es) (mkSortedVertices vs')

addEdge :: UndirGraph -> UndirEdge -> Maybe UndirGraph
addEdge (UG (SE es) (SV vs)) e = let es' = e : es in mkUndirGraph es' vs

pick2distinct :: SortedVertices -> [ UndirEdge ] 
pick2distinct = filter (not . isSelfish) . pick2

pick2 :: SortedVertices -> [ UndirEdge ] 
pick2 (SV vs) = (map (uncurry UndirEdge) . concatMap f . tails) vs
  where f :: [Vertex] -> [(Vertex,Vertex)] 
        f [] = []
        f lst@(x:xs) = map (x,) lst

generate1EdgeMore :: UndirGraph -> [UndirGraph] 
generate1EdgeMore gr = (mapMaybe (addEdge gr) . pick2 . vertices) gr