{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.EnumeratedSeqZipper where

import GHC.TypeLits
--
-- import Control.Applicative hiding (empty)
-- import Data.Foldable
-- import Data.Monoid ((<>))
-- import Data.Sequence 
import           Data.Singletons
-- import Data.Traversable
import Data.Type.Equality
--
import           Data.EnumeratedSequence hiding (singleton)
-- import qualified Data.EnumeratedSequence as N (singleton)
-- 
-- import Prelude hiding (zipWith, length, splitAt)

-- |
data NSeqZipper' (m :: MNat) (n :: MNat) a where
  --        current   lefts        rights
  NSZ :: a -> NSeq' m a -> NSeq' n a -> NSeqZipper' m n a

type NSeqZipper (m :: Nat) (n :: Nat) = NSeqZipper' (FromNat m) (FromNat n) 

-- |
singleton :: a -> NSeqZipper 0 0 a  
singleton x = NSZ x empty empty

 
 
-- | 
first :: forall m n a . (SingI m, SingI n) => 
         NSeqZipper' m n a -> NSeqZipper' MZero (m :+: n) a 
first z@(NSZ x ls rs) = 
  case sing :: Sing m of
    MyZero -> z
    MySucc p -> case viewl (MySucc p) ls of 
                  y :< ys -> gcastWith (plus_succ_r p (sing :: Sing n)) (NSZ y empty (ys >< (x <| rs))) 


--  :: NSeq' (m :+: n) a) 

                    
{- 


MyZero _ z = z 
first (MySucc p) n (NSZ x ls rs) = 
    case viewl (MySucc p) ls of 
      y :< ys -> gcastWith (plus_succ_r p n) (NSZ y empty (ys >< (x <| rs))) --  :: NSeq' (m :+: n) a) 
-}

{-
-- |
last :: SeqZipper a -> SeqZipper a 
last orig@(SZ (x,(x1s,x2s))) = 
  case viewr x2s of 
    EmptyR -> orig
    zs :> z -> SZ (z,((x1s |> x) `mappend` zs , empty))

-- | 
moveL :: SeqZipper a -> Maybe (SeqZipper a)
moveL (SZ (x,(x1s,x2s))) = 
  case viewr x1s of
    EmptyR -> Nothing 
    zs :> z -> Just (SZ (z,(zs,x<|x2s)))

-- |
moveR :: SeqZipper a -> Maybe (SeqZipper a) 
moveR (SZ (x,(x1s,x2s))) = 
  case viewl x2s of 
    EmptyL -> Nothing
    z :< zs -> Just (SZ (z,(x1s|>x,zs)))

------------------------
-- fetch focused item -- 
------------------------

-- | 
current :: SeqZipper a -> a 
current (SZ (x,(_,_))) = x

-- | 
prev :: SeqZipper a -> Maybe a 
prev = fmap current . moveLeft

-- |
next :: SeqZipper a -> Maybe a 
next = fmap current . moveRight

------------------------------
-- operation on focues item --
------------------------------

-- |
replace :: a -> SeqZipper a -> SeqZipper a 
replace y (SZ (_x,zs)) = SZ (y,zs)

-- |
delete :: SeqZipper a -> Maybe (SeqZipper a)
delete (SZ (_,(xs,ys))) = 
  case viewl ys of 
    EmptyL -> case viewr xs of 
                EmptyR -> Nothing 
                zs :> z -> Just (SZ (z,(zs,ys)))
    z :< zs -> Just (SZ (z,(xs,zs)))
-}

-- toSeq :: SeqZipper a -> Seq a
-- toSeq (SZ (x,(x1s,x2s))) = x1s >< (x <| x2s)


{- 
-- |
deriving instance Functor SeqZipper

-- |
deriving instance Foldable SeqZipper 

-- |
instance Applicative SeqZipper where
  pure = singletonSZ 
  SZ (f,(f1s,f2s)) <*> SZ (x,(y1s,y2s)) = SZ (f x, (zipWith id f1s y1s, zipWith id f2s y2s))

-- |
deriving instance Traversable SeqZipper 

-- |
lengthSZ :: SeqZipper n a -> Int 
lengthSZ (SZ (_x, (x1s,x2s))) = length x1s + length x2s + 1 

-- |
currIndex :: SeqZipper a -> Int
currIndex (SZ (_x, (x1s,_x2s))) = length x1s 


-- |
appendGoLast :: SeqZipper a -> a -> SeqZipper a
appendGoLast (SZ (y,(y1s,y2s))) x = SZ (x, ((y1s |> y) >< y2s, empty))

-- |
chopFirst :: SeqZipper a -> Maybe (SeqZipper a)
chopFirst (SZ (y,(y1s,y2s))) = 
  case viewl y1s of
    EmptyL -> case viewl y2s of 
                EmptyL -> Nothing 
                z :< zs -> Just (SZ (z,(empty,zs)))
    _z :< zs -> Just (SZ (y,(zs,y2s)))
    

-- |
moveTo :: Int -> SeqZipper a -> Maybe (SeqZipper a) 
moveTo n orig@(SZ (x,(x1s,x2s))) = 
  let n_x1s = length x1s 
      n_x2s = length x2s 
      res | n < 0 || n > n_x1s + n_x2s = Nothing 
          | n == n_x1s = Just orig 
          | n < n_x1s = let (x1s1, x1s2) = splitAt n x1s 
                            el :< rm = viewl x1s2
                        in Just (SZ (el, (x1s1,(rm |> x) >< x2s)))
          | n > n_x1s = let (x2s1,x2s2) = splitAt (n-n_x1s-1) x2s
                            el :< rm = viewl x2s2
                        in Just (SZ (el, ((x1s |> x) >< x2s1, rm)))
          | otherwise = error "error in moveTo"
  in res 




-}