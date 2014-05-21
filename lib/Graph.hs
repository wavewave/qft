{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Graph where

import           GHC.TypeLits
-- 
import           Data.Array
import           Data.Maybe (mapMaybe)
import           Data.List (sortBy )
-- import           Data.Proxy
-- 
import           Permute
-- 
import Prelude hiding (lookup)


type Vertex n = Within n

data UndirEdge n = UE { edgeV1 :: Vertex n
                      , edgeV2 :: Vertex n }
                 deriving (Show, Eq)

verticesFromEdge :: UndirEdge n -> [Vertex n]
verticesFromEdge (UE x y) = [x,y]

isSelfish :: UndirEdge n -> Bool
isSelfish (UE x y) = x == y 
 
edgecmp :: UndirEdge n -> UndirEdge n -> Ordering
edgecmp (UE x1 x2) (UE y1 y2) = compare (x1,x2) (y1,y2)  

mkUndirEdge :: Vertex n -> Vertex n -> UndirEdge n 
mkUndirEdge x y | x <= y = UE x y 
                | otherwise = UE y x 

connectedVertex :: Vertex n -> UndirEdge n -> Maybe (Vertex n)
connectedVertex v (UE x y) 
    | v == x = Just y 
    | v == y = Just x 
    | otherwise = Nothing

{- 
data SortedVertices n = SV { unSV :: [Vertex n] }
                      deriving (Show, Eq)

mkSortedVertices :: [Vertex n] -> SortedVertices n
mkSortedVertices = SV . nub . sort 
-}

data SortedEdges n = SE { unSE :: [UndirEdge n] }
                   deriving (Show, Eq)

mkSortedEdges :: [UndirEdge n] -> SortedEdges n
mkSortedEdges xs = SE (sortBy edgecmp xs) 

newtype UndirGraph n = UG { edges :: SortedEdges n }
--                       , vertices :: SortedVertices }
                deriving (Show, Eq)

mkUndirGraph :: [UndirEdge n] {- -> [Vertex n] -> -} -> UndirGraph n
mkUndirGraph es {- vs -} = UG (mkSortedEdges es)
{-
  let v1s = (concatMap verticesFromEdge) es
      b = all (`elem` vs) v1s 
  in if b then Just (UG (mkSortedEdges es) (mkSortedVertices vs)) else Nothing
-}

permuteEdge :: Permutation n -> UndirEdge n -> UndirEdge n
permuteEdge p (UE v1 v2) = mkUndirEdge (permute p v1) (permute p v2)

permuteGraph :: Permutation n -> UndirGraph n -> UndirGraph n 
permuteGraph p (UG (SE es)) = UG (mkSortedEdges (map (permuteEdge p) es)) 



{-
class GetOrder a where
  getOrder :: a -> Integer

class GetMax a where
  type ValueType a :: * 
  getMax :: a -> ValueType a

instance (KnownNat n) => GetOrder (UndirGraph n) where
  getOrder (_ :: UndirGraph n) = natVal (Proxy :: Proxy n)

instance (KnownNat n) => GetMax (UndirGraph n) where
  type ValueType (UndirGraph n) = Vertex n
  getMax _ = mkWithinMod (natVal (Proxy :: Proxy n))
-}

type AssocMap n = Array (Vertex n) [Vertex n]

mkAssocMap :: (KnownNat n) => UndirGraph n -> AssocMap n
mkAssocMap g@(UG (SE es)) = let alst = map (\v -> (v, mapMaybe (connectedVertex v) es)) (interval g)
                                    in array (1,order g) alst 
 
degree :: AssocMap n -> [ Vertex n ] -> Vertex n -> Int
degree arr ptn i = length (filter (`elem` ptn) (arr ! i))


