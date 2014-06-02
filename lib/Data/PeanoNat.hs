{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.PeanoNat where

import GHC.TypeLits
-- 
import Data.Singletons
import Data.Type.Equality

data MNat = MZero | MSucc MNat

type family n1 :+: n2 where
  MZero :+: n2 = n2
  (MSucc n1') :+: n2 = MSucc (n1' :+: n2)

data instance Sing (a :: MNat) where
  MyZero :: Sing MZero
  MySucc :: Sing n -> Sing (MSucc n)

instance SingI MZero where 
  sing = MyZero 

instance (SingI n) => SingI (MSucc n) where
  sing = MySucc (sing :: Sing n)

type family FromNat (n :: Nat) where
  FromNat 0 = MZero
  FromNat n = MSucc (FromNat (n-1))

plus_id_r :: forall (n :: MNat) . Sing n -> ((n :+: MZero) :~: n)
plus_id_r MyZero = Refl
plus_id_r (MySucc n) = gcastWith (plus_id_r n) Refl  

plus_succ_r :: forall (n1 :: MNat) (n2 :: MNat) . Sing n1 -> Sing n2 -> (n1 :+: (MSucc n2)) :~: (MSucc (n1 :+: n2))
plus_succ_r MyZero _  = Refl
plus_succ_r (MySucc n1) n2 = gcastWith (plus_succ_r n1 n2) Refl


