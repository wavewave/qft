module Util where

--------------------
-- either utility --
--------------------

-- |
guardEither :: String -> Bool -> Either String ()
guardEither str False = Left str
guardEither _ True = Right ()
 
-- |
maybeEither :: String -> Maybe a -> Either String a
maybeEither str Nothing = Left str
maybeEither _ (Just x) = Right x

---------------------
-- tuple functions --
---------------------

-- |
fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c
