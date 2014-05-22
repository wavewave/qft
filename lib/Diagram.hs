{-# LANGUAGE ScopedTypeVariables #-}

module Diagram where

import           GHC.TypeLits
--
-- import           Data.Array
import           Data.Hashable
import qualified Data.HashSet as H
-- import           Data.Graph
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
                     } deriving (Show, Eq, Ord)

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



-- listArray (1,fromInteger (intValue order)) lst 
isCompatibleWith :: (KnownNat n) => AssocMap n -> VertexKindSet -> Bool
isCompatibleWith asc vkinds = let d1 = H.map vertexKindDeg vkinds 
                                  d2 = (H.fromList . map fst . globalVertexDegree) asc
                              in H.null (H.difference d2 d1)  

                      
