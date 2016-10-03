{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.BList.Nat (
    Nat (..)
) where

-- | Inductive natural number.
data Nat = O | S Nat

infixl 6 .+.
infixl 7 .*.

-- | addition operation for natural number.
type family (n :: Nat) .+. (m :: Nat) :: Nat
type instance O .+. m     = m
type instance (S n) .+. m = S (n .+. m)

-- | multiplication operation for natural number.
type family (n :: Nat) .*. (m :: Nat) :: Nat
type instance O .*. m     = O
type instance (S n) .*. m = (n .*. m) .+. m
