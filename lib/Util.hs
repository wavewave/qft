module Util where

-- |
guardEither :: String -> Bool -> Either String ()
guardEither str False = Left str
guardEither _ True = Right ()
 
-- |
maybeEither :: String -> Maybe a -> Either String a
maybeEither str Nothing = Left str
maybeEither _ (Just x) = Right x
