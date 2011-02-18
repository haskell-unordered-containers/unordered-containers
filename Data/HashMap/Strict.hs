{-# LANGUAGE BangPatterns #-}

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

import Data.Hashable (Hashable)
import Prelude hiding (filter, foldr, lookup, map, null)

import Data.HashMap.Lazy hiding (insert, singleton)
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
