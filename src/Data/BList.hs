{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.BList (
      BList (..)
    , empty
    , singleton
    , (++)
    , toList
    , fromList
) where

import Prelude hiding ((++))
import Data.BList.Nat

-- | Type-safe list.
data BList a n =
    (n ~ O) => Nil
    | forall m. n ~ S m => (:+) a (BList a m)

infixr 5 :+

-- | Construct an empty BList.
empty :: BList a O
empty = Nil

-- | Construct a BList from only one element.
singleton :: a -> BList a (S O)
singleton x = x :+ Nil

-- | Append operation.
(++) :: BList a n -> BList a m -> BList a (n .+. m)
(++) Nil ts     = ts
(++) (x:+xs) ts = x :+ (++) xs ts

infixr 5 ++

-- | Transform BList to ordinary list.
toList :: BList a n -> [a]
toList Nil     = []
toList (x:+xs) = x : toList xs

-- | Transform ordinary list to BList.
fromList :: [a] -> BList a n
fromList = undefined -- foldr (\x l -> x :+ l) Nil
