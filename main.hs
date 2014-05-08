module Main where

data Symbol = A | B | C 
     deriving (Show,Eq,Ord)

data Index = I | J | K | L | M | N
           deriving (Show,Eq,Ord)
-- type Scalar = Symbol

data Tensor = Tensor { symbol :: Symbol, indices :: [Index] }
            deriving (Show,Eq,Ord)

data Monomial = Monomial [Tensor]
              deriving (Show,Eq,Ord)

a = Tensor A [I,J]

b = Tensor B [K,L] 

mon1 = Monomial [a,b]

monomial2Tensor :: Symbol -> Monomial -> Tensor
monomial2Tensor s (Monomial xs) = Tensor s (concatMap indices xs)


main :: IO ()
main = do 
  putStrLn "Haha, I am the omnipotent QFT solver!"

  print mon1  
  print (monomial2Tensor C mon1)