{-# LANGUAGE BangPatterns, CPP #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.FullList.Strict
-- Copyright   :  2010-2011 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Non-empty lists of key/value pairs.  The lists are strict in the
-- keys and the values.

module Data.FullList.Strict
    ( FullList

      -- * Basic interface
    , size
    , singleton
    , lookup
    , insert
    , delete
    , insertWith
    , adjust

      -- * Transformations
    , map
    , traverseWithKey

      -- * Folds
    , foldlWithKey'
    , foldrWithKey

      -- * Filter
    , filterWithKey
    ) where

import Prelude hiding (lookup, map)

import Data.FullList.Lazy hiding (insertWith, map, adjust)
import Data.Sized

insertWith :: Eq k => (v -> v -> v) -> k -> v -> FullList k v
           -> Sized (FullList k v)
insertWith f !k v (FL k' v' xs)
    | k == k'   = let v'' = f v v' in v'' `seq` 0 :!: FL k v'' xs
    | otherwise = case insertWithL f k v xs of
        sz :!: ys -> sz :!: FL k' v' ys
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

insertWithL :: Eq k => (v -> v -> v) -> k -> v -> List k v -> Sized (List k v)
insertWithL = go
  where
    go _ !k v Nil = 1 :!: Cons k v Nil
    go f k v (Cons k' v' xs)
        | k == k'   = let v'' = f v v' in v'' `seq` 0 :!: Cons k v'' xs
        | otherwise = case go f k v xs of
            sz :!: ys -> sz :!: Cons k' v' ys
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWithL #-}
#endif

adjust :: Eq k => (v -> v) -> k -> FullList k v -> FullList k v
adjust f !k (FL k' v xs)
  | k == k' = let v' = f v in v' `seq` FL k' v' xs
  | otherwise = FL k' v (adjustL f k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#endif

adjustL :: Eq k => (v -> v) -> k -> List k v -> List k v
adjustL f = go
  where
    go !_ Nil = Nil
    go k (Cons k' v xs)
      | k == k' = let v' = f v in v' `seq` Cons k' v' xs
      | otherwise = Cons k' v (go k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjustL #-}
#endif

------------------------------------------------------------------------
-- * Transformations

map :: (k1 -> v1 -> (k2, v2)) -> FullList k1 v1 -> FullList k2 v2
map f (FL k v xs) = let !(k', !v') = f k v
                    in FL k' v' (mapL f xs)
{-# INLINE map #-}

mapL :: (k1 -> v1 -> (k2, v2)) -> List k1 v1 -> List k2 v2
mapL f = go
  where
    go Nil = Nil
    go (Cons k v xs) = let !(k', !v') = f k v
                       in Cons k' v' (go xs)
{-# INLINE mapL #-}
