{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Array
import Data.Fin1
import Data.Permute
-- 

main = do 
  putStrLn "permutation test"
  r <- runEitherT $ do 
    p1 <- hoistEither (fromTuple (1,2,4,3,5)) -- (mkPerm arr1)
    p2 <- hoistEither (fromTuple (1,3,2,4,5)) -- (mkPerm arr2)
    let  p = p1 · p2 
         g = 2

    (gen :: Generator 2 5) <- hoistEither (mkGen (listArray (1,2) [p, p1]))
                  
    liftIO $ do 
      print (firstUnfixed p1)
      print (firstUnfixed p)
      print (chooseUnfixed gen)
      print (splitFixed gen)
    return ()

  either print (const (return ())) r  
    
              
  