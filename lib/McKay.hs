module McKay where

import           GHC.TypeLits
--
-- import           Control.Applicative
import qualified Data.Foldable as F
import           Data.List (delete)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Sequence (fromList, (<|), empty)
import           Data.Tree
--
import           Data.Partition
import           Data.SeqZipper
-- import           Data.Within  
import Graph 
-- import Permute
-- 
-- import Debug.Trace

shatteringBy :: AssocMap n -> [Vertex n] -> [Vertex n] -> [ [Vertex n] ] 
shatteringBy arr vi vj = let resultmap = foldr f M.empty vi
                             asclst = M.toAscList resultmap 
                         in map ((flip ($) []) . snd) asclst
  where  
    f x acc = let d = degree arr vj x
              in M.insertWith (\n o ->  n . o)  d (x:) acc 

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


equitableRefinement :: AssocMap n -> OrderedPartition n -> OrderedPartition n 
equitableRefinement asc optn = go optn
  where go x = case shatter asc x of
                 [] -> x
                 y:_ -> go y 


splittingBy :: OrderedPartition n -> Vertex n -> OrderedPartition n 
splittingBy ptn x = let SZ (oldpart, (ys,zs)) = locateInPartition ptn x
                        newpart = [x] <| delete x oldpart <| empty 
                    in OP (ys <> newpart <> zs)


type SearchTree n = Tree (OrderedPartition n) 

 
createSearchTree :: (KnownNat n) => AssocMap n -> SearchTree n 
createSearchTree asc = worker unitPartition
  where worker x = let y = equitableRefinement asc x 
                   in maybe (Node y []) (f y) (firstNontrivial y)
        f y (SZ (c,_)) = Node y (map (worker . splittingBy y)  c)

   
