{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Graph where

import           GHC.TypeLits
-- 
import           Data.Array
import           Data.Hashable
import           Data.Maybe (mapMaybe)
import           Data.List (sortBy )
-- 
import           Data.Permute
import           Data.Within
-- 
import Prelude hiding (lookup)

-- |
type Vertex n = Within n

-- |
data UndirEdge n = UE { edgeV1 :: Vertex n
                      , edgeV2 :: Vertex n }
                 deriving (Show, Eq)

instance (KnownNat n) => Hashable (UndirEdge n) where
  hashWithSalt salt (UE v1 v2) = hashWithSalt salt (v1,v2)

-- |
verticesFromEdge :: UndirEdge n -> [Vertex n]
verticesFromEdge (UE x y) = [x,y]

-- |
isSelfish :: UndirEdge n -> Bool
isSelfish (UE x y) = x == y 
 
-- |
edgecmp :: UndirEdge n -> UndirEdge n -> Ordering
edgecmp (UE x1 x2) (UE y1 y2) = compare (x1,x2) (y1,y2)  

-- |
mkUndirEdge :: Vertex n -> Vertex n -> UndirEdge n 
mkUndirEdge x y | x <= y = UE x y 
                | otherwise = UE y x 

-- |
connectedVertex :: Vertex n -> UndirEdge n -> Maybe (Vertex n)
connectedVertex v (UE x y) 
    | v == x = Just y 
    | v == y = Just x 
    | otherwise = Nothing

-- |
data SortedEdges n = SE { unSE :: [UndirEdge n] }
                   deriving (Show, Eq)

instance (KnownNat n) => Hashable (SortedEdges n) where
  hashWithSalt salt (SE xs) = hashWithSalt salt xs

-- |
mkSortedEdges :: [UndirEdge n] -> SortedEdges n
mkSortedEdges xs = SE (sortBy edgecmp xs) 

-- |
newtype UndirGraph n = UG { edges :: SortedEdges n }
                deriving (Show, Eq)

instance (KnownNat n) => Hashable (UndirGraph n) where
  hashWithSalt salt (UG xs) = hashWithSalt salt xs

-- |
mkUndirGraph :: [UndirEdge n] -> UndirGraph n
mkUndirGraph es = UG (mkSortedEdges es)

-- |
permuteEdge :: Permutation n -> UndirEdge n -> UndirEdge n
permuteEdge p (UE v1 v2) = mkUndirEdge (permute p v1) (permute p v2)

-- |
permuteGraph :: Permutation n -> UndirGraph n -> UndirGraph n 
permuteGraph p (UG (SE es)) = UG (mkSortedEdges (map (permuteEdge p) es)) 

-- |
type AssocMap n = Array (Vertex n) [Vertex n]

-- |
mkAssocMap :: (KnownNat n) => UndirGraph n -> AssocMap n
mkAssocMap (UG (SE es)) = let alst = map (\v -> (v, mapMaybe (connectedVertex v) es)) interval 
                          in array (1,order) alst 

-- | 
degree :: AssocMap n -> [ Vertex n ] -> Vertex n -> Int
degree arr ptn i = length (filter (`elem` ptn) (arr ! i))


