{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Diagram where

import           GHC.TypeLits
--
import           Control.Applicative
-- import           Data.Array
import           Data.Hashable
import qualified Data.HashSet as H
-- import           Data.Graph
import           Data.List (find)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Traversable (sequenceA)
--
-- import           Data.Within
import           Graph
import           McKay

-- | direction of line (in,out,undirected)
data Direction = I | O | U deriving (Show,Eq,Ord)

instance Hashable Direction where
  hashWithSalt salt dir = case dir of 
                            I -> hashWithSalt salt (1 :: Int)
                            O -> hashWithSalt salt (2 :: Int)
                            U -> hashWithSalt salt (3 :: Int)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

data VertexKind = VK { vertexKindId :: Int
                     , vertexKindName :: String
                     , vertexKindEdgeList :: [ (Int, Direction, Int) ]    -- (edge id, direction, multiplicity)
                     } deriving (Eq, Ord)

instance Show VertexKind where
  show = vertexKindName

vertexKindDeg :: VertexKind -> Int
vertexKindDeg = sum . map trd3 . vertexKindEdgeList

instance Hashable VertexKind where
  hashWithSalt salt (VK i n d) = hashWithSalt salt (i,n,d)

data EdgeKind = EK { edgeKindId :: Int
                   , edgeKindName :: String
                   , edgeKindIsDirected :: Bool 
                   } 

type VertexKindSet = H.HashSet VertexKind

type EdgeKindSet = H.HashSet EdgeKind


-- |
isCompatibleWith :: (KnownNat n) => VertexKindSet -> AssocMap n -> Bool
isCompatibleWith vkinds asc = let d1 = H.map vertexKindDeg vkinds 
                                  d2 = (H.fromList . map fst . globalVertexDegree) asc
                              in H.null (H.difference d2 d1)   
                      
-- |
vertexCandidates :: (KnownNat n) => VertexKindSet -> AssocMap n -> [ (VertexKind, [Vertex n]) ] 
vertexCandidates vkinds asc = (mapMaybe f . H.toList) vkinds 
  where dlst = globalVertexDegree asc
        f x = let d = vertexKindDeg x
                  mx = find ((== d) . fst) dlst
              in (\y->(x,snd y)) <$> mx

-- |
transpose :: (Eq a, Eq b, Ord a, Ord b) => [ (a, [b]) ] -> [ (b, [a])  ]
transpose olst = let lst1 = [ (k,v) | (k,vs) <- olst, v <- vs ] 
                     f (k,v) = M.insertWith (.) v (k:) 
                     map2 = foldr f M.empty lst1 
                 in (M.toAscList . fmap ((flip ($) []) )) map2 


generateVertexMapping :: (KnownNat n) => VertexKindSet -> AssocMap n -> [ [ (Vertex n, VertexKind) ] ] -- -> [ Map (Within n) String ] 
generateVertexMapping vkinds asc = sequenceA vks
  where vc = transpose (vertexCandidates vkinds asc)
        f (v,ks) = [(v,k)| k <- ks ] 
        vks = map f vc

vertexMapToString :: [ (Vertex n, VertexKind) ] -> M.Map (Vertex n) String
vertexMapToString = fmap vertexKindName  . M.fromList 
