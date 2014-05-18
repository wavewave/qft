{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module OldTopology
( Vertex
, UndirEdge(edgeV1, edgeV2)
, UndirGraph(vertices, edges)
, SortedVertices(unSV)
, SortedEdges(unSE)
, CanonicalUndirGraph(getGraph)
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
, reindex
, canonicalize
, graphOrder
, vertexOrder
, permute
, sortVertex
) where

import Control.Applicative
import Control.Monad
import Data.Function (on)
import Data.List (nub, sort, sortBy, tails, lookup,group )
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Permute as P
-- 
import Debug.Trace
import Prelude hiding (lookup)


type Vertex = Int

data UndirEdge = UndirEdge { edgeV1 :: Int
                           , edgeV2 :: Int }
               deriving (Show, Eq)

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
                    deriving (Show, Eq)

mkSortedVertices :: [Int] -> SortedVertices 
mkSortedVertices = SV . nub . sort 

data SortedEdges = SE { unSE :: [UndirEdge] }
                 deriving (Show, Eq)

mkSortedEdges :: [UndirEdge] -> SortedEdges
mkSortedEdges xs = SE (sortBy edgecmp xs) 

data UndirGraph = UG { edges :: SortedEdges
                     , vertices :: SortedVertices
                     }
                deriving (Show, Eq)

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


newtype CanonicalUndirGraph = CanonicalUndirGraph { getGraph :: UndirGraph }
                            deriving (Show, Eq)

reindex :: (Vertex -> Vertex) -> UndirEdge -> UndirEdge
reindex f UndirEdge {..} = mkUndirEdge (f edgeV1) (f edgeV2)

canonicalize :: UndirGraph -> CanonicalUndirGraph
canonicalize go =  
    let pairs = zip (unSV (vertices go)) [0..]
        f x = fromJust (lookup x pairs)
        es = (map (reindex f) . unSE . edges) go
        mgr = mkUndirGraph es (map snd pairs)
    in (CanonicalUndirGraph  . fromJust) mgr

 
graphOrder :: UndirGraph -> Int
graphOrder = length . unSV . vertices


vertexOrder :: UndirGraph -> [(Vertex,Int)]
vertexOrder g = let vs = (unSV . vertices) g
                    es = (unSE . edges) g
                    vs_edge = sort (foldr f [] es)
                    counts = (map ((,) <$> head <*> length) . group) vs_edge
                in map (\x -> maybe (x,0) (x,) (lookup x counts)) vs
  where f x acc = let r1:r2:[] = verticesFromEdge x in r1:r2:acc

permute :: P.Permute -> CanonicalUndirGraph -> Maybe CanonicalUndirGraph
permute p g = do 
    let ug = getGraph g
    guard (P.size p == graphOrder ug)
    let e' = (map (reindex (P.at p)) . unSE . edges) ug
    g' <- (mkUndirGraph e' . unSV . vertices) ug
    return (CanonicalUndirGraph g')

sortVertex :: CanonicalUndirGraph -> CanonicalUndirGraph
sortVertex g = let ug = getGraph g
                   n = graphOrder ug
                   vo = vertexOrder ug 
                   v' = map fst (sortBy (flip compare `on` snd) vo)
                   p = P.inverse (P.listPermute n v')
               in fromJust (permute p g)
                        

    

