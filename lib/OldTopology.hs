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


addVertex :: UndirGraph -> Vertex -> UndirGraph
addVertex (UG (SE es) (SV vs)) v = let vs' = v : vs in UG (SE es) (mkSortedVertices vs')



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
                        

    

