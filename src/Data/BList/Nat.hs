{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.BList.Nat (
      module Data.BList.Nat
    , sing
    , SingI
) where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH (singletons)
import Prelude (Num (..), Eq(..), Show(..))
import qualified Prelude

-- | Inductive natural number and promote it with singletons.
singletons [d|
    data Nat = O | S Nat deriving (Eq, Show)
    |]

singletons [d|
    instance Prelude.Num Nat where
        O   + n = n
        S m + n = S (m + n)

        n   - O   = n
        S n - S m = n - m
        O   - S _ = O

        O   * _ = O
        S n * m = n * m + m

        abs n = n

        signum O = O
        signum (S _) = S O

        fromInteger n = if n == 0 then O else S (fromInteger (n-1))
    |]

-- | Addition, minus, multiplication and exponentiation for origin numbers.
type n .-. m = n :- m
type n .+. m = n :+ m
type n .*. m = n :* m

infixl 6 .+.
infixl 6 .-.
infixl 7 .*.

-- | Addition for singleton numbers.
(.:+) :: SNat n -> SNat m -> SNat (n .+. m)
(.:+) = (%:+)

-- | Minus for singleton numbers.
(.:-) :: SNat n -> SNat m -> SNat (n .-. m)
(.:-) = (%:-)

-- | Multiplication for singleton numbers.
(.:*) :: SNat n -> SNat m -> SNat (n .*. m)
(.:*) = (%:*)

infixl 6 .:+
infixl 6 .:-
infixl 7 .:*
