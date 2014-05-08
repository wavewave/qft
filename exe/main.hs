module Main where

import Control.Monad
import Control.Monad.Trans.State
import Data.List (nub,sort)

newtype Symbol = Symbol { unSymbol :: String }
               deriving (Show,Eq,Ord)

newtype Index = Index { unIndex :: String }
              deriving (Show,Eq,Ord)

data Tensor = Tensor { symbol :: Symbol, indices :: [Index] }
            deriving (Show,Eq,Ord)

data Associator = Associator { asymbol :: Symbol, aindices :: [Index] } 
                deriving (Show,Eq,Ord)



data Product = Product { tensor :: [Tensor] 
                       , assoc :: [Associator]  }
               deriving (Show,Eq,Ord)

newtype Sum = Sum { unSum :: [Product] }
         deriving (Show,Eq,Ord)


data Graph = Graph { tensorlist :: [Tensor] 
                   , edgelist :: [Edge] } 

data Edge = Edge { eindex :: Index
                 , esymbol1 ::  Symbol 
                 , esymbol2 :: Symbol }

mkEdge :: Index -> Symbol -> Symbol -> Edge
mkEdge idx s1 s2 | s1 < s2 = Edge idx s1 s2
                 | otherwise = Edge idx s2 s1


graph2Product :: Graph -> Maybe Product
graph2Product g = do 
    guard (checkSinglet g)
    flip evalStateT [] $ do 
      let tlst = tensorlist g
          eidxlst = (map eindex . edgelist) g 
          alst = map (\x->deltan [Index (unIndex x++"1")] [Index (unIndex x ++ "2")]) eidxlst 
          findRegistered :: Index -> StateT [Index] Maybe Bool
          findRegistered idx = do
            registered <- get
            if (idx `elem` registered) then return True else put (idx:registered) >> return False
          renameIdx :: Index -> StateT [Index] Maybe Index
          renameIdx idx = do 
            b <- findRegistered idx
            if b then (return . Index) (unIndex idx ++ "2") else (return . Index) (unIndex idx ++ "1")
      tlst' <- mapM (\t -> (mapM renameIdx . indices) t >>= \is -> return (t {indices = is})) tlst 
      return (Product tlst' alst)
      


s = Symbol
i = Index 

a = Tensor (s "X") [i "I"]

b = Tensor (s "Y") [i "J"]

c = Tensor (s "X") [i "K"]

xl = Tensor (s "X") [i "L"]

zero = Associator (s "0") [] 

deltan idx1 idx2 = Associator (s "deltaN") (idx1 ++ idx2)

prod1 = Product [a,b,c] [] 


yukawa :: Graph
yukawa = Graph [ Tensor (s "y") [i "I", i "J"]
               , Tensor (s "Q") [i "alpha", i "I"]
               , Tensor (s "U") [i "beta", i "J"]
               , Tensor (s "H") []
               , Tensor (s "Gamma") [i "alpha", i "beta"]
               ] 
               [ mkEdge (i "I") (s "y") (s "Q")
               , mkEdge (i "J") (s "y") (s "U")
               , mkEdge (i "alpha") (s "Q") (s "Gamma")
               , mkEdge (i "beta") (s "U") (s "Gamma")
               ] 

checkSinglet :: Graph -> Bool 
checkSinglet g = (sort .  concatMap indices . tensorlist) g == (sort . merge . dup . map eindex . edgelist) g

merge :: [(a,a)] -> [a]
merge ((x1,x2):xs) = x1:x2:merge xs
merge [] = []

dup :: [a] -> [(a,a)]
dup xs = zip xs xs 

-- dupMerge :: [a] -> [a]
-- dupMerge = merge  . dup -- (zip xs xs)

diffTensor :: Tensor -> Tensor -> Associator
diffTensor x y  
  | symbol x == symbol y && length (indices x) == length (indices y) = deltan (indices x) (indices y)
  | otherwise = zero   



diff :: Product -> Tensor -> Sum
diff (Product ts as) x = (Sum . filter (not . isZero) . map f  .getHoles []) ts
  where f (p,(q1,q2)) = Product (q1++q2) (diffTensor p x : as) 


isZero :: Product -> Bool
isZero = any ((== "0") . unSymbol . asymbol) . assoc   


-- | find holes: for example [x,y,z] to [(x,([],[y,z])), (y,([x],[z])), (z,([y,x],[]))] 
getHoles :: [a] -> [a] -> [(a,([a],[a]))]
getHoles ys [] = error "no element" 
getHoles ys (x:[]) = [(x,(ys,[]))]
getHoles ys (x:xs) = (x,(ys,xs)) : getHoles (ys++[x]) xs

promoteTensor :: Tensor -> Product
promoteTensor t = Product [t] []

product2Tensor :: Symbol -> Product -> Tensor
product2Tensor s (Product xs ys) = Tensor s resultIdx
  where primIdx = concatMap indices xs
        assocIdx = concatMap aindices ys
        resultIdx = filter (not . (`elem` assocIdx)) primIdx

main :: IO ()
main = do 
  print prod1  
  print (product2Tensor (s "C") prod1)

  print (getHoles [] [1,2,3,4,5])
  (mapM_ print . unSum . diff prod1) xl

  putStrLn "------__"

  print $ concatMap indices (tensorlist yukawa)

  print (checkSinglet yukawa)

  print (graph2Product yukawa)