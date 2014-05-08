module Main where

data Symbol = Symbol Char -- A | B | C | D | E | F
     deriving (Show,Eq,Ord)

data Index = Index Char -- I | J | K | L | M | N
           deriving (Show,Eq,Ord)
-- type Scalar = Symbol

data Tensor = Tensor { symbol :: Symbol, indices :: [Index] }
            deriving (Show,Eq,Ord)

-- data Monomial = Monomial [Tensor]
--               deriving (Show,Eq,Ord)

data Associator = Associator { asymbol :: Symbol, aindices :: [Index] } 
                deriving (Show,Eq,Ord)



data Product = Product [Tensor] [Associator]
               deriving (Show,Eq,Ord)
   

s = Symbol
i = Index 

a = Tensor (s 'A') [i 'I',i 'J']

b = Tensor (s 'B') [i 'K',i 'L'] 

delta = Associator (s 'D') [i 'J',i 'K'] 


prod1 = Product [a,b] [delta]

product2Tensor :: Symbol -> Product -> Tensor
product2Tensor s (Product xs ys) = Tensor s resultIdx
  where primIdx = concatMap indices xs
        assocIdx = concatMap aindices ys
      
        resultIdx = filter (not . (`elem` assocIdx)) primIdx


main :: IO ()
main = do 
  putStrLn "Haha, I am the omnipotent QFT solver!"

  print prod1  
  print (product2Tensor (s 'C') prod1)