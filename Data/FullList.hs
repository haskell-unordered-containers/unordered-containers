{-# LANGUAGE BangPatterns, CPP #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.FullList
-- Copyright   :  2010 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Non-empty lists of key/value pairs.

module Data.FullList
    ( FullList

      -- * Basic interface
    , size
    , singleton
    , lookup
    , insert
    , delete
    , adjustWithDefault

      -- * Transformations
    , map

      -- * Folds
    , foldl'
    , foldr

      -- * Filter
    , filter
    ) where

import Control.DeepSeq (NFData(rnf))
import Prelude hiding (filter, foldr, lookup, map)

------------------------------------------------------------------------
-- * The 'FullList' type

-- The 'FullList' type has two benefits:
--
--  * it is guaranteed to be non-empty, and
--
--  * it can be unpacked into a data constructor.

-- Invariant: the same key only appears once in a 'FullList'.

-- | A non-empty list of key/value pairs.
data FullList k v = FL !k !v !(List k v)
                  deriving Show

instance (Eq k, Eq v) => Eq (FullList k v) where
    (FL k1 v1 xs) == (FL k2 v2 ys) = k1 == k2 && v1 == v2 && xs == ys
    (FL k1 v1 xs) /= (FL k2 v2 ys) = k1 /= k2 || v1 /= v2 || xs /= ys

instance (NFData k, NFData v) => NFData (FullList k v)

data List k v = Nil | Cons !k !v !(List k v)
              deriving Show

instance (Eq k, Eq v) => Eq (List k v) where
    (Cons k1 v1 xs) == (Cons k2 v2 ys) = k1 == k2 && v1 == v2 && xs == ys
    Nil == Nil = True
    _   == _   = False

    (Cons k1 v1 xs) /= (Cons k2 v2 ys) = k1 /= k2 || v1 /= v2 || xs /= ys
    Nil /= Nil = False
    _   /= _   = True

instance (NFData k, NFData v) => NFData (List k v) where
    rnf Nil           = ()
    rnf (Cons k v xs) = rnf k `seq` rnf v `seq` rnf xs

-- TODO: Check if evaluation is forced.

------------------------------------------------------------------------
-- * FullList

-- The 'List' functions are not inlined as they should be seldomly
-- called in practice (i.e. we expect few collisions.)

size :: FullList k v -> Int
size (FL _ _ xs) = 1 + sizeL xs

sizeL :: List k v -> Int
sizeL Nil = 0
sizeL (Cons _ _ xs) = 1 + sizeL xs

singleton :: k -> v -> FullList k v
singleton k v = FL k v Nil

lookup :: Eq k => k -> FullList k v -> Maybe v
lookup !k (FL k' v xs)
    | k == k'   = Just v
    | otherwise = lookupL k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

lookupL :: Eq k => k -> List k v -> Maybe v
lookupL = go
  where
    go !_ Nil = Nothing
    go k (Cons k' v xs)
        | k == k'   = Just v
        | otherwise = go k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookupL #-}
#endif

insert :: Eq k => k -> v -> FullList k v -> FullList k v
insert !k v (FL k' v' xs)
    | k == k'   = FL k v xs
    | otherwise = FL k' v' (insertL k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(n)/ Insert at the head of the list to avoid copying the whole
-- list.
insertL :: Eq k => k -> v -> List k v -> List k v
insertL = go
  where
    go !k v Nil = Cons k v Nil
    go k v (Cons k' v' xs)
        | k == k'   = Cons k v xs
        | otherwise = Cons k' v' (go k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertL #-}
#endif

delete :: Eq k => k -> FullList k v -> Maybe (FullList k v)
delete !k (FL k' v xs)
    | k == k'   = case xs of
        Nil             -> Nothing
        Cons k'' v' xs' -> Just $ FL k'' v' xs'
    | otherwise = let ys = deleteL k xs
                  in ys `seq` Just (FL k' v ys)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

deleteL :: Eq k => k -> List k v -> List k v
deleteL = go
  where
    go !_ Nil = Nil
    go k (Cons k' v xs)
        | k == k'   = xs
        | otherwise = Cons k' v (go k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteL #-}
#endif

adjustWithDefault :: Eq k => (v -> v) -> k -> v -> FullList k v -> FullList k v
adjustWithDefault f !k v (FL k' v' xs)
    | k == k'   = FL k (f v') xs
    | otherwise = FL k' v' (adjustWithDefaultL f k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjustWithDefault #-}
#endif

adjustWithDefaultL :: Eq k => (v -> v) -> k -> v -> List k v -> List k v
adjustWithDefaultL = go
  where
    go _ !k v Nil = Cons k v Nil
    go f k v (Cons k' v' xs)
        | k == k'   = Cons k (f v') xs
        | otherwise = Cons k' v' (go f k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjustWithDefaultL #-}
#endif

------------------------------------------------------------------------
-- * Transformations

map :: (k1 -> v1 -> (k2, v2)) -> FullList k1 v1 -> FullList k2 v2
map f (FL k v xs) = let (k', v') = f k v
                    in FL k' v' (mapL f xs)
{-# INLINE map #-}

mapL :: (k1 -> v1 -> (k2, v2)) -> List k1 v1 -> List k2 v2
mapL f = go
  where
    go Nil = Nil
    go (Cons k v xs) = let (k', v') = f k v
                       in Cons k' v' (go xs)
{-# INLINE mapL #-}

------------------------------------------------------------------------
-- * Folds

foldl' :: (a -> k -> v -> a) -> a -> FullList k v -> a
foldl' f !z (FL k v xs) = foldl'L f (f z k v) xs
{-# INLINE foldl' #-}

foldl'L :: (a -> k -> v -> a) -> a -> List k v -> a
foldl'L f = go
  where
    go !z Nil          = z
    go z (Cons k v xs) = go (f z k v) xs
{-# INLINE foldl'L #-}

foldr :: (k -> v -> a -> a) -> a -> FullList k v -> a
foldr f z (FL k v xs) = f k v (foldrL f z xs)
{-# INLINE foldr #-}

foldrL :: (k -> v -> a -> a) -> a -> List k v -> a
foldrL f = go
  where
    go z Nil = z
    go z (Cons k v xs) = f k v (go z xs)
{-# INLINE foldrL #-}

------------------------------------------------------------------------
-- * Filter

filter :: (k -> v -> Bool) -> FullList k v -> Maybe (FullList k v)
filter p (FL k v xs)
    | p k v     = Just (FL k v ys)
    | otherwise = case ys of
        Nil           -> Nothing
        Cons k' v' zs -> Just $ FL k' v' zs
  where !ys = filterL p xs
{-# INLINE filter #-}

filterL :: (k -> v -> Bool) -> List k v -> List k v
filterL p = go
  where
    go Nil = Nil
    go (Cons k v xs)
        | p k v     = Cons k v (go xs)
        | otherwise = go xs
{-# INLINE filterL #-}
