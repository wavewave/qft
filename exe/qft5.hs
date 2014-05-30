{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array
import Data.Fin1
import Data.Permute
-- 
import Prelude hiding ((^^))

main = do 
  putStrLn "permutation test"
  let arr1 = listArray (1,5) [1,2,4,3,5] :: Array (Fin1 5) (Fin1 5)
      arr2 = listArray (1,5) [1,3,2,4,5] :: Array (Fin1 5) (Fin1 5)
      er = do p1 <- mkPerm arr1
              p2 <- mkPerm arr2 
              let  p = p1 · p2 
                   g = 2
                   α' = 3 :: Int

              (gen :: Generator 1 5) <- mkGen (listArray (1,1) [p1  ] )  
                  
              -- return (p · (p^-), g ↙ p )
              -- return (splitFixed gen) 
              return (chooseUnfixed gen)
  
              
  print er