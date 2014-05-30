{-# LANGUAGE DataKinds #-}

import Data.Array
import Data.Fin1
import Data.Permute

main = do 
  putStrLn "permutation test"
  let arr1 = listArray (1,5) [1,2,4,3,5] :: Array (Fin1 5) (Fin1 5)
      arr2 = listArray (1,5) [1,3,2,4,5] :: Array (Fin1 5) (Fin1 5)
      er = do p1 <- mkPerm arr1
              p2 <- mkPerm arr2 
              let  p = p1 `mult` p2 
              
              return (p `mult` inverse p)
  
  print er