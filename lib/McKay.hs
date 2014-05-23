{-# LANGUAGE ScopedTypeVariables #-}

module McKay where

import           GHC.TypeLits
--
import qualified Data.Foldable as F
import           Data.List (delete)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Sequence (fromList, (<|), empty)
import           Data.Tree
--
import           Data.Partition
import           Data.Permute
import           Data.SeqZipper
import           Data.Within
import Graph 
-- 

shatteringBy :: AssocMap n -> [Vertex n] -> [Vertex n] -> [ [Vertex n] ] 
shatteringBy arr vi vj = let resultmap = foldr f M.empty vi
                             asclst = M.toAscList resultmap 
                         in map ((flip ($) []) . snd) asclst
  where  
    f x acc = let d = degree arr vj x
              in M.insertWith (.)  d (x:) acc 

-- |
globalVertexDegree :: forall n. (KnownNat n) => AssocMap n -> [ (Int, [Vertex n] ) ]
globalVertexDegree arr = map deg (shatteringBy arr u u)
  where u = interval
        deg vs = let v = head vs -- we know that this is safe as a result of shatteringBy 
                 in (degree arr u v, vs)

-- |
shatter :: AssocMap n -> OrderedPartition n -> [OrderedPartition n]
shatter arr optn = concatMap (\vizip -> mapMaybe (shatterwork vizip) ptnlst) (zippers optn)
  where ptn = getPartition optn
        ptnlst = F.toList ptn
        shatterwork vizip vj = 
          let SZ (x,(ys,zs)) = vizip
          in case shatteringBy arr x vj of
              [] -> error "empty partition?"  -- this is errored case but guaranteed not exist
              _:[] -> Nothing
              xs -> Just (OP (ys <> fromList xs <> zs))

-- |
equitableRefinement :: AssocMap n -> OrderedPartition n -> OrderedPartition n 
equitableRefinement asc optn = go optn
  where go x = case shatter asc x of
                 [] -> x
                 y:_ -> go y 

-- |
splittingBy :: OrderedPartition n -> Vertex n -> OrderedPartition n 
splittingBy ptn x = let SZ (oldpart, (ys,zs)) = locateInPartition ptn x
                        newpart = [x] <| delete x oldpart <| empty 
                    in OP (ys <> newpart <> zs)

-- |
type SearchTree n = Tree (OrderedPartition n) 

-- | 
createSearchTree :: (KnownNat n) => OrderedPartition n -> AssocMap n -> SearchTree n 
createSearchTree ptn asc = worker ptn -- unitPartition
  where worker x = let y = equitableRefinement asc x 
                   in maybe (Node y []) (f y) (firstNontrivial y)
        f y (SZ (c,_)) = Node y (map (worker . splittingBy y)  c)

-- |   
isomorphisms :: (KnownNat n) => SearchTree n -> [Permutation n]
isomorphisms = catMaybes . fmap (fmap inverse . discreteToPermutation) . flatten

-- |
canonicalLabel :: (KnownNat n) => OrderedPartition n -> UndirGraph n -> UndirGraph n
canonicalLabel ptn g = let asc = mkAssocMap g
                           tree = createSearchTree ptn asc
                           isos = isomorphisms tree
                       in permuteGraph (head isos) g  -- guaranteed to exist at list one

