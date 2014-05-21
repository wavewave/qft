{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Topology.Generate where

import GHC.TypeLits
--
import Data.HashSet (HashSet)
import Data.List (tails)
import Data.Proxy 
--  
import Data.Within
import Graph

type TopologySet n = HashSet (UndirGraph n)

addEdge :: UndirGraph n -> UndirEdge n  -> UndirGraph n
addEdge (UG (SE es)) e = let es' = e : es in mkUndirGraph es' 

pick2distinct :: (KnownNat n) => [ UndirEdge n ] 
pick2distinct = filter (not . isSelfish) pick2

pick2 :: forall n. (KnownNat n) => [ UndirEdge n ] 
pick2 = (map (uncurry UE) . concatMap f . tails) (interval (Proxy :: Proxy n))
  where f :: [Vertex n] -> [(Vertex n,Vertex n)] 
        f [] = []
        f lst@(x:_) = map (x,) lst

generate1EdgeMore :: (KnownNat n) => UndirGraph n -> [UndirGraph n] 
generate1EdgeMore g = map (addEdge g) pick2

generate1EdgeMore' :: (KnownNat n) => UndirGraph n -> [UndirGraph n] 
generate1EdgeMore' g = map (addEdge g) pick2distinct

