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
import           Data.Singletons
import Data.Type.Equality
--
import           Data.Enumerated.Sequence hiding (singleton)
import           Data.PeanoNat
-- 

-- |
data NSeqZipper' (m :: PNat) (n :: PNat) a where
  --     current    lefts        rights
  NSZ ::       a -> NSeq' m a -> NSeq' n a -> NSeqZipper' m n a

-- | to use type-level literal natural number
type NSeqZipper (m :: Nat) (n :: Nat) = NSeqZipper' (FromNat m) (FromNat n) 

-- |
singleton :: a -> NSeqZipper 0 0 a  
singleton x = NSZ x empty empty

-- | 
toFirst :: forall m n a . (SingI m, SingI n) => 
         NSeqZipper' m n a -> NSeqZipper' PZero (m :+: n) a 
toFirst z@(NSZ x ls rs) = 
  case sing :: Sing m of
    PSZero -> z
    PSSucc p -> case viewl (PSSucc p) ls of 
                  y :< ys -> gcastWith (plus_succ_r p (sing :: Sing n)) (NSZ y empty (ys >< (x <| rs))) 


-- |
toLast :: forall m n a. (SingI m, SingI n) => 
          NSeqZipper' m n a -> NSeqZipper' (m :+: n) PZero a 
toLast z@(NSZ x ls rs) = 
  case sing :: Sing n of
    PSZero -> gcastWith (plus_id_r (sing :: Sing m)) z
    PSSucc p -> case viewr (PSSucc p) rs of
                  ys :> y -> gcastWith (plus_succ_r p (sing :: Sing m)) $ 
                             NSZ y (ls >< (x <| ys)) empty

-- | 
toLeft :: forall m n a. (SingI m, SingI n) => 
          NSeqZipper' (PSucc m) n a -> NSeqZipper' m (PSucc n) a
toLeft (NSZ x ls rs) = let lview = viewr (sing :: Sing (PSucc m)) ls  
                       in case lview of
                            ys :> y -> NSZ y ys (x <| rs) 

-- |
toRight :: forall m n a. (SingI m, SingI n) => 
           NSeqZipper' m (PSucc n) a -> NSeqZipper' (PSucc m) n a
toRight (NSZ x ls rs) = let rview = viewl (sing :: Sing (PSucc n)) rs
                        in case rview of 
                             y :< ys -> NSZ y (ls |> x) ys

------------------------
-- fetch focused item -- 
------------------------

-- | 
current :: NSeqZipper' m n a -> a 
current (NSZ x _ _) = x

-- | 
prev :: (SingI m, SingI n) => NSeqZipper' (PSucc m) n a -> a 
prev = current . toLeft

-- |
next :: (SingI m, SingI n) => NSeqZipper' m (PSucc n) a -> a
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
  case viewl ys of 
    EmptyL -> case viewr xs of 
                EmptyR -> Nothing 
                zs :> z -> Just (SZ (z,(zs,ys)))
    z :< zs -> Just (SZ (z,(xs,zs)))
-}

-- toSeq :: SeqZipper a -> Seq a
-- toSeq (SZ (x,(x1s,x2s))) = x1s >< (x <| x2s)


