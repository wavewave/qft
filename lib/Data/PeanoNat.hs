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

data PNat = PZero | PSucc PNat

type family n1 :+: n2 where
  PZero :+: n2 = n2
  (PSucc n1') :+: n2 = PSucc (n1' :+: n2)

data instance Sing (a :: PNat) where
  PSZero :: Sing PZero
  PSSucc :: Sing n -> Sing (PSucc n)

instance SingI PZero where 
  sing = PSZero 

instance (SingI n) => SingI (PSucc n) where
  sing = PSSucc (sing :: Sing n)

type family FromNat (n :: Nat) where
  FromNat 0 = PZero
  FromNat n = PSucc (FromNat (n-1))

------------------
-- basic proofs --
------------------

-- | n + 0 = n
plus_id_r :: forall (n :: PNat) . Sing n -> ((n :+: PZero) :~: n)
plus_id_r PSZero = Refl
plus_id_r (PSSucc n) = gcastWith (plus_id_r n) Refl  

-- | n1+(n2+1) = (n1+n2)+1
plus_succ_r :: forall (n1 :: PNat) (n2 :: PNat) . Sing n1 -> Sing n2 -> (n1 :+: (PSucc n2)) :~: (PSucc (n1 :+: n2))
plus_succ_r PSZero _  = Refl
plus_succ_r (PSSucc n1) n2 = gcastWith (plus_succ_r n1 n2) Refl


