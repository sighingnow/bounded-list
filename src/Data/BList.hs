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
    , head
    , tail
    , replicate
) where

import Prelude hiding ((++), head, tail, replicate)
import Data.BList.Nat

-- | Type-safe list.
data BList n a =
    (n ~ O) => Nil
    | forall m. n ~ S m => (:+) a (BList m a)

infixr 5 :+

instance (Show a) => Show (BList n a) where
    show = show . toList

instance (Eq a) => Eq (BList n a) where
    (==) Nil Nil         = True
    (==) (x:+xs) (y:+ys) = x == y && xs == ys

instance Foldable (BList n) where
    foldMap f Nil     = mempty
    foldMap f (x:+xs) = mappend (f x) (foldMap f xs)

-- | Construct an empty BList.
empty :: BList O a
empty = Nil

-- | Construct a BList from only one element.
singleton :: a -> BList (S O) a
singleton x = x :+ Nil

-- | Append operation.
(++) :: BList n a -> BList m a -> BList (n .+. m) a
(++) Nil ts     = ts
(++) (x:+xs) ts = x :+ (++) xs ts

-- | Transform BList to ordinary list.
toList :: BList n a -> [a]
toList Nil     = []
toList (x:+xs) = x : toList xs

-- | Transform ordinary list to BList.
fromList :: [a] -> BList n a
fromList = undefined -- foldr (\x l -> x :+ l) Nil

-- | Type-safe head.
head :: BList (S n) a -> a
head (x:+_) = x

-- | Type-safe tail.
tail :: BList (S n) a -> BList n a
tail (_:+xs) = xs

-- | 'replicate' @n x@ is a BList of length @n@ with @x@ the value of
-- every element.
replicate :: SNat n -> a -> BList n a
replicate SO _     = Nil
replicate (SS n) a = a :+ replicate n a

