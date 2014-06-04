{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
-- import Data.Array
--
import Data.Enumerated.Partition
import Data.Enumerated.Sequence
import Data.Fin1
import Data.FromTuple
import Data.PeanoNat
import Data.Permute
-- 

main = do 
  putStrLn "permutation test"
  r <- runEitherT $ do 
    p1 <- hoistEither (fromTuple (1,2,4,3,5)) -- (mkPerm arr1)
    p2 <- hoistEither (fromTuple (1,3,2,4,5)) -- (mkPerm arr2)
    let  p = p1 · p2 
         g = 2

    (gen :: Generator 2 5) <- hoistEither (mkGen ( p <| p1 <| empty ))
                  
    liftIO $ do 
      print (firstUnfixed p1)
      print (firstUnfixed p)
      print (chooseUnfixed gen)
      print (splitFixed gen)
  
    -- NSeq test
    lst1 :: NSeq 2 Int <- hoistEither (fromTuple (1,2))
    lst2 :: NSeq 3 Int <- hoistEither (fromTuple (3,4,5))
    liftIO $ do 
      print lst1
      print lst2
      print (singleton 0 `Cons` lst1 `Cons` lst2 `Cons` Nil)
  

      -- print (Nil :: Partition '[] Int) 
      print (singleton 0 `PCons` lst1 `PCons` lst2 `PCons` PNil :: Partition [ '(FromNat 1, NSeq 1 Int) 
                                                                             , '(FromNat 2, NSeq 2 Int)
                                                                             , '(FromNat 3, NSeq 3 Int) ] )

    return ()

  either print (const (return ())) r  
    
              
  