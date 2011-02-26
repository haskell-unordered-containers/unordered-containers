{-# LANGUAGE BangPatterns, CPP #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Strict
-- Copyright   :  2010-2011 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A map from /hashable/ keys to values.  A map cannot contain
-- duplicate keys; each key can map to at most one value.  A 'HashMap'
-- makes no guarantees as to the order of its elements.
--
-- This map is strict in both the keys and the values; keys and values
-- are evaluated to /weak head normal form/ before they are added to
-- the map.  Exception: the provided instances are the same as for the
-- lazy version of this module.
--
-- The implementation is based on /big-endian patricia trees/, keyed
-- by a hash of the original key.  A 'HashMap' is often faster than
-- other tree-based maps, especially when key comparison is expensive,
-- as in the case of strings.
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).

module Data.HashMap.Strict
    (
      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , lookup
    , insert
    , delete
    , insertWith
    , adjust

      -- * Transformations
    , map
    , traverseWithKey

      -- * Folds
    , foldl'
    , foldlWithKey'
    , foldr
    , foldrWithKey

      -- * Filter
    , filter
    , filterWithKey

      -- * Conversions
    , elems
    , keys

      -- ** Lists
    , toList
    , fromList

      -- * Behavioral metadata
    , collisions
    , collisionHistogram
    ) where

import Data.Hashable (Hashable(hash))
import Prelude hiding (filter, foldr, lookup, map, null)

import qualified Data.FullList.Strict as FL
import Data.HashMap.Common
import Data.HashMap.Lazy hiding (fromList, insert, insertWith, adjust, map, singleton)
import qualified Data.HashMap.Lazy as L
import qualified Data.List as List

------------------------------------------------------------------------
-- * Basic interface

-- | /O(1)/ Construct a map with a single element.
singleton :: Hashable k => k -> v -> HashMap k v
singleton k !v = L.singleton k v
{-# INLINE singleton #-}

-- | /O(min(n,W))/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k0 !v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin s m l r)
        | nomatch h s m = join h (Tip h $ FL.singleton k v) s t
        | zero h m      = Bin s m (go h k v l) r
        | otherwise     = Bin s m l (go h k v r)
    go h k v t@(Tip h' l)
        | h == h'       = Tip h $ FL.insert k v l
        | otherwise     = join h (Tip h $ FL.singleton k v) h' t
    go h k v Nil        = Tip h $ FL.singleton k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(min(n,W))/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
           -> HashMap k v
insertWith f k0 !v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin s m l r)
        | nomatch h s m = join h (Tip h $ FL.singleton k v) s t
        | zero h m      = Bin s m (go h k v l) r
        | otherwise     = Bin s m l (go h k v r)
    go h k v t@(Tip h' l)
        | h == h'       = Tip h $ FL.insertWith f k v l
        | otherwise     = join h (Tip h $ FL.singleton k v) h' t
    go h k v Nil        = Tip h $ FL.singleton k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

-- | /O(min(n,W)/ Adjust the value tied to a given key in this map
-- only if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k0 t0 = go h0 k0 t0
  where
    h0 = hash k0
    go !h !k t@(Bin p m l r)
      | nomatch h p m = t
      | zero h m      = Bin p m (go h k l) r
      | otherwise     = Bin p m l (go h k r)
    go h k t@(Tip h' l)
      | h == h'       = Tip h $ FL.adjust f k l
      | otherwise     = t
    go _ _ Nil        = Nil
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#endif


------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go (Bin s m l r) = Bin s m (go l) (go r)
    go (Tip h l)     = Tip h (FL.map f' l)
    go Nil           = Nil
    f' k v = (k, f v)
{-# INLINE map #-}

------------------------------------------------------------------------
-- Conversions

-- | /O(n*min(W, n))/ Construct a map from a list of elements.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = List.foldl' (\ m (k, v) -> insert k v m) empty
{-# INLINE fromList #-}
