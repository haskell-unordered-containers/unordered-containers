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
-- the map.
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

      -- * Transformations
    , map

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
    ) where

import Data.Hashable (Hashable(hash))
import Prelude hiding (filter, foldr, lookup, map, null)

import qualified Data.FullList.Strict as FL
import Data.HashMap.Common
import Data.HashMap.Lazy hiding (insert, insertWith, singleton)
import qualified Data.HashMap.Lazy as L

-- | /O(1)/ Construct a map with a single element.
singleton :: Hashable k => k -> v -> HashMap k v
singleton k !v = L.singleton k v
{-# INLINE singleton #-}

-- | /O(min(n,W))/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k !v = L.insert k v

-- | /O(min(n,W))/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
           -> HashMap k v
insertWith f k0 v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin p m l r)
        | nomatch h p m = join h (Tip h $ FL.singleton k v) p t
        | zero h m      = Bin p m (go h k v l) r
        | otherwise     = Bin p m l (go h k v r)
    go h k v t@(Tip h' l)
        | h == h'       = Tip h $ FL.insertWith f k v l
        | otherwise     = join h (Tip h $ FL.singleton k v) h' t
    go h k v Nil        = Tip h $ FL.singleton k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif
