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
    , Sing
    , SingI
) where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH (singletons)
import Prelude (id, error)
import Prelude (Num (..), Eq(..), Show(..), Ord(..), Ordering(..), Bool(..))
import qualified Prelude

-- | Inductive natural number and promote it with singletons.
singletons [d|
    data Nat = O | S Nat deriving (Eq, Show)
    |]

singletons [d|
    instance Num Nat where
        O   + n = n
        S m + n = S (m + n)

        n   - O   = n
        S n - S m = n - m
        O   - S _ = O

        O   * _ = O
        S n * m = n * m + m

        abs n = id n

        signum n = id n

        fromInteger n | n == 0 = O
                      | n > 0 = S (fromInteger (n-1))
                      | n < 0 = error "Nat.fromInteger: can't construct negative nature number."
    |]

singletons [d|
    instance Ord Nat where
        O `compare` O = EQ
        O `compare` S _ = LT
        S _ `compare` O = GT
        S a `compare` S b = a `compare` b

        O <= O = True
        O <= S _ = True
        S _ <= O = False
        S a <= S b = a <= b
    |]

-- | Addition, minus, multiplication and exponentiation for natural numbers.
type n .-. m = n :- m
type n .+. m = n :+ m
type n .*. m = n :* m

-- | Comparing operators for natural numbers.
type n .<. m = n :< m
type n .<=. m = n :<= m
type n .>. m = n :> m
type n .>=. m = n :>= m
type n .==. m = n :== m

-- | Addition for singleton numbers.
(.:+) :: SNat n -> SNat m -> SNat (n .+. m)
(.:+) = (%:+)

-- | Minus for singleton numbers.
(.:-) :: SNat n -> SNat m -> SNat (n .-. m)
(.:-) = (%:-)

-- | Multiplication for singleton numbers.
(.:*) :: SNat n -> SNat m -> SNat (n .*. m)
(.:*) = (%:*)

infix 4 .<., .<=., .>., .>=., .==.
infix 6 .+., .-., .:+, .:-
infix 7 .*., .:*
