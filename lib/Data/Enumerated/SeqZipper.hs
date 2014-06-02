{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Enumerated.SeqZipper where

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
import           Data.Enumerated.Sequence hiding (singleton)
-- import qualified Data.EnumeratedSequence as N (singleton)
import           Data.PeanoNat
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
toFirst :: forall m n a . (SingI m, SingI n) => 
         NSeqZipper' m n a -> NSeqZipper' MZero (m :+: n) a 
toFirst z@(NSZ x ls rs) = 
  case sing :: Sing m of
    MyZero -> z
    MySucc p -> case viewl (MySucc p) ls of 
                  y :< ys -> gcastWith (plus_succ_r p (sing :: Sing n)) (NSZ y empty (ys >< (x <| rs))) 


-- |
toLast :: forall m n a. (SingI m, SingI n) => 
          NSeqZipper' m n a -> NSeqZipper' (m :+: n) MZero a 
toLast z@(NSZ x ls rs) = 
  case sing :: Sing n of
    MyZero -> gcastWith (plus_id_r (sing :: Sing m)) z
    MySucc p -> case viewr (MySucc p) rs of
                  ys :> y -> gcastWith (plus_succ_r p (sing :: Sing m)) $ 
                             NSZ y (ls >< (x <| ys)) empty

-- | 
toLeft :: forall m n a. (SingI m, SingI n) => 
          NSeqZipper' (MSucc m) n a -> NSeqZipper' m (MSucc n) a
toLeft (NSZ x ls rs) = let lview = viewr (sing :: Sing (MSucc m)) ls  
                       in case lview of
                            ys :> y -> NSZ y ys (x <| rs) 

-- |
toRight :: forall m n a. (SingI m, SingI n) => 
           NSeqZipper' m (MSucc n) a -> NSeqZipper' (MSucc m) n a
toRight (NSZ x ls rs) = let rview = viewl (sing :: Sing (MSucc n)) rs
                        in case rview of 
                             y :< ys -> NSZ y (ls |> x) ys


------------------------
-- fetch focused item -- 
------------------------

-- | 
current :: NSeqZipper' m n a -> a 
current (NSZ x _ _) = x

-- | 
prev :: (SingI m, SingI n) => NSeqZipper' (MSucc m) n a -> a 
prev = current . toLeft

-- |
next :: (SingI m, SingI n) => NSeqZipper' m (MSucc n) a -> a
next = current . toRight


------------------------------
-- operation on focues item --
------------------------------

-- |
modify :: (a -> a) -> NSeqZipper' m n a -> NSeqZipper' m n a
modify f (NSZ x ls rs) = NSZ (f x) ls rs

-- |
replace :: a -> NSeqZipper' m n a -> NSeqZipper' m n a 
replace x = modify (const x)

{- difficult
-- |
delete :: forall m n m' n' a. (SingI m, SingI n, SingI m', SingI n') => NSeqZipper' m n a -> NSeqZipper' m' n' a
delete (NSZ _ ls rs) = 
  case sing



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