module McKay where

import           Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Sequence (ViewL(..), viewl,fromList)
--
import           Data.Partition
import           Data.SeqZipper
-- import           Data.Within  
import Graph 
-- import Permute

shatteringBy :: AssocMap n -> [Vertex n] -> [Vertex n] -> [ [Vertex n] ] 
shatteringBy arr vi vj = let resultmap = foldr f M.empty vj 
                             asclst = M.toAscList resultmap 
                         in map ((flip ($) []) . snd) asclst
  where  
    f x acc = let d = degree arr vi x
              in M.insertWith (\n o ->  n . o)  d (x:) acc 

shatter :: AssocMap n -> OrderedPartition n -> [OrderedPartition n]
shatter arr optn = concatMap (\vizip -> mapMaybe (shatterwork vizip) ptnlst) zippers
  where ptn = getPartition optn
        ptnlst = F.toList ptn
        ptn1 = case viewl ptn of
                 EmptyL -> error "impossble" -- guaranteed from OrderedPartition and n >= 1
                 x :< xs -> fromNonEmptySeq (x,xs)
        zippers = (catMaybes . takeWhile (isJust) . iterate (moveRight =<<)) (pure ptn1)
        shatterwork vizip vj = 
          case shatteringBy arr (current vizip) vj of
            [] -> error "empty partition?"  -- this is errored case but guaranteed not exist
            _x:[] -> Nothing
            xs -> let SZ (_,(ys,zs)) = vizip 
                  in Just (OP (ys <> fromList xs <> zs))


equitableRefinement :: AssocMap n -> OrderedPartition n -> OrderedPartition n 
equitableRefinement asc optn = go optn
  where go x = case shatter asc x of
                 [] -> x
                 y:_ -> go y



 -- (replace xs vizip)

