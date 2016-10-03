{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.BList.Nat (
      Nat (..)
    , type (.+.)
    , type (.*.)
    , SNat (..)
    , (.:+)
    , (.:*)
) where

-- | Inductive natural number.
data Nat = O | S Nat

-- | Addition operation for natural number.
type family (n :: Nat) .+. (m :: Nat) :: Nat
type instance O .+. m     = m
type instance (S n) .+. m = S (n .+. m)

infixl 6 .+.

-- | Multiplication operation for natural number.
type family (n :: Nat) .*. (m :: Nat) :: Nat
type instance O .*. m     = O
type instance (S n) .*. m = (n .*. m) .+. m

infixl 7 .*.

-- | Using singleton to promoting the origin Nat type.
data SNat n where
    SO :: SNat O
    SS :: SNat n -> SNat (S n)

-- | Addition operation for singleton Nat type.
(.:+) :: SNat n -> SNat m -> SNat (n .+. m)
SO   .:+ m = m
SS n .:+ m = SS (n .:+ m)

infixl 6 .:+

-- | Multiplication operation for singleton Nat type.
(.:*) :: SNat n -> SNat m -> SNat (n .*. m)
SO   .:* m = SO
SS n .:* m = (n .:* m) .:+ m

infixl 7 .:*
