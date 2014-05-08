module Main where

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


data Exp = ExpSimpl Tensor
         | ExpProd Product  

s = Symbol
i = Index 

a = Tensor (s "X") [i "I"]

b = Tensor (s "Y") [i "J"]

c = Tensor (s "X") [i "K"]

xl = Tensor (s "X") [i "L"]

zero = Associator (s "0") [] 

deltan idx1 idx2 = Associator (s "deltaN") (idx1 ++ idx2)

-- delta = Associator (s "D") [i "J",i "K"] 

diffTensor :: Tensor -> Tensor -> Associator
diffTensor x y  
  | symbol x == symbol y && length (indices x) == length (indices y) = deltan (indices x) (indices y)
  | otherwise = zero   


prod1 = Product [a,b,c] [] --  [delta]

diff :: Product -> Tensor -> [Product]
diff (Product ts as) x = (filter (not . isZero) . map f  .getHoles []) ts
  where f (p,(q1,q2)) = Product (q1++q2) (diffTensor p x : as) 


 -- undefined -- map (Pro  . (\x -> diffTensor x y) ts 

isZero :: Product -> Bool
isZero = any ((== "0") . unSymbol . asymbol) . assoc   


-- | find holes: for example [x,y,z] to [(x,([],[y,z])), (y,([x],[z])), (z,([y,x],[]))] 
getHoles :: [a] -> [a] -> [(a,([a],[a]))]
getHoles ys [] = error "no element" 
getHoles ys (x:[]) = [(x,(ys,[]))]
getHoles ys (x:xs) = (x,(ys,xs)) : getHoles (ys++[x]) xs

-- next (xs,y:ys) = Just (y, (xs,ys))
-- next (xs,[]) = Nothing





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
  mapM_ print (diff prod1 xl)