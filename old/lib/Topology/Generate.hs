{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Topology.Generate where

import GHC.TypeLits
--
import           Data.Graph (scc, Tree(..) )
import qualified Data.HashSet as H
import Data.List (tails)
--  
import Data.Partition (unitPartition)
import Data.Fin1
import Graph
import McKay

type TopologySet n = H.HashSet (UndirGraph n)

addEdge :: UndirGraph n -> UndirEdge n  -> UndirGraph n
addEdge (UG (SE es)) e = let es' = e : es in mkUndirGraph es' 

pick2distinct :: (KnownNat n) => [ UndirEdge n ] 
pick2distinct = filter (not . isSelfish) pick2

pick2 :: forall n. (KnownNat n) => [ UndirEdge n ] 
pick2 = (map (uncurry UE) . concatMap f . tails) interval
  where f :: [Vertex n] -> [(Vertex n,Vertex n)] 
        f [] = []
        f lst@(x:_) = map (x,) lst

generate1EdgeMore :: (KnownNat n) => UndirGraph n -> [UndirGraph n] 
generate1EdgeMore g = map (addEdge g) pick2

generate1EdgeMore' :: (KnownNat n) => UndirGraph n -> [UndirGraph n] 
generate1EdgeMore' g = map (addEdge g) pick2distinct



nextEdgeLevel :: (KnownNat n) => TopologySet n -> TopologySet n
nextEdgeLevel = foldr H.insert H.empty . map (canonicalLabel unitPartition) . concatMap generate1EdgeMore' . H.toList


nextEdgeLevelConnected :: (KnownNat n) => TopologySet n -> TopologySet n
nextEdgeLevelConnected = H.filter ((<=1) . length . filter (not . isIsolated) . scc . undirToDirected) . nextEdgeLevel
  where isIsolated (Node _ []) = True
        isIsolated _ = False
