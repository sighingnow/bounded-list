{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.BList (
      BList (..)
    , empty
    , singleton
    , (++)
    , toList
    , fromList
    , fromList'
    , replicate
    , replicate'
    , head
    , tail
    , take
    , drop
    , slice
) where

import Prelude hiding ((++), head, tail, replicate, take, drop)

import Data.BList.Nat

-- | Type-safe list.
data BList n a =
    (n ~ 'O) => Nil
    | forall m. n ~ 'S m => (:+) a (BList m a)

infixr 5 :+

instance (Show a) => Show (BList n a) where
    show = show . toList

instance (Eq a) => Eq (BList n a) where
    (==) Nil Nil         = True
    (==) (x:+xs) (y:+ys) = x == y && xs == ys

instance Foldable (BList n) where
    foldMap _ Nil     = mempty
    foldMap f (x:+xs) = mappend (f x) (foldMap f xs)

-- | Construct an empty BList.
empty :: BList 'O a
empty = Nil

-- | Construct a BList from only one element.
singleton :: a -> BList ('S 'O) a
singleton x = x :+ Nil

-- | Append operation.
(++) :: BList n a -> BList m a -> BList (n .+. m) a
(++) Nil ts     = ts
(++) (x:+xs) ts = x :+ (++) xs ts

-- | Transform BList to ordinary list.
toList :: BList n a -> [a]
toList Nil     = []
toList (x:+xs) = x : toList xs

-- | Transform ordinary list to BList with type of length given.
fromList' :: SNat n -> [a] -> BList n a
fromList' SO [] = Nil
fromList' (SS n) (x:xs) = x :+ fromList' n xs
fromList' _ _ = error "BList.fromList': length dismatch, impossible!"

-- | Transform ordinary list to BList with type of length inferred.
fromList :: SingI n => [a] -> BList n a
fromList = fromList' sing

-- | 'replicate' @n x@ is a BList of length @n@ with @x@ the value of
-- every element.
replicate' :: SNat n -> a -> BList n a
replicate' SO _     = Nil
replicate' (SS n) a = a :+ replicate' n a

-- | 'replicate' @x@ for times which can be inferred from context. For example:
--
-- > replicate 3 :: BList ('S ('S 'O)) Int
-- > [3,3]
replicate :: SingI n => a -> BList n a
replicate = replicate' sing

-- | Type-safe head.
head :: BList ('S n) a -> a
head (x:+_) = x

-- | Type-safe tail.
tail :: BList ('S n) a -> BList n a
tail (_:+xs) = xs

-- | Type-safe take
--
-- > take (SS (SS SO)) xs.
take :: ((m .<=. n) ~ 'True) => SNat m -> BList n a -> BList m a
take SO _ = Nil
take (SS n) (x :+ xs) = x :+ take n xs

-- | Type-safe take.
drop :: ((m .<=. n) ~ 'True) => SNat m -> BList n a -> BList (n .-. m) a
drop = undefined

-- | Type-safe slice.
slice :: ((i .<=. j) ~ 'True, (j .<=. n) ~ 'True)
    => Sing (i :: Nat)
    -> Sing (j :: Nat)
    -> BList n a
    -> BList (j .-. i) a
slice SO j vec = take j vec
slice (SS i) (SS j) (_ :+ xs) = slice i j xs
