{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Array
import Data.Fin1
import Data.Permute
-- 
import Prelude hiding ((^^))

main = do 
  putStrLn "permutation test"
  let arr1 = listArray (1,5) [1,2,4,3,5] :: Array (Fin1 5) (Fin1 5)
      arr2 = listArray (1,5) [1,3,2,4,5] :: Array (Fin1 5) (Fin1 5)


  r <- runEitherT $ do 
    p1 <- hoistEither (mkPerm arr1)
    p2 <- hoistEither (mkPerm arr2)
    let  p = p1 · p2 
         g = 2

    (gen :: Generator 1 5) <- hoistEither (mkGen (listArray (1,1) [p1]))
                  
    liftIO $ print (firstUnfixed p1)
    liftIO $ print (firstUnfixed p)
    liftIO (print (chooseUnfixed gen))
    return ()

  either print (const (return ())) r  
    
              
  