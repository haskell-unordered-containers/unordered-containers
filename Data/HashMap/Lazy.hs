{-# LANGUAGE BangPatterns, CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Lazy
-- Copyright   :  2010-2012 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A map from /hashable/ keys to values.  A map cannot contain
-- duplicate keys; each key can map to at most one value.  A 'HashMap'
-- makes no guarantees as to the order of its elements.
--
-- This map is strict in the keys and lazy in the values; keys are
-- evaluated to /weak head normal form/ before they are added to the
-- map.
--
-- The implementation is based on /hash array mapped tries/.  A
-- 'HashMap' is often faster than other tree-based set types,
-- especially when key comparison is expensive, as in the case of
-- strings.
--
-- Many operations have a average-case complexity of /O(log n)/.  The
-- implementation uses a large base (i.e. 16) so in practice these
-- operations are constant time.
module Data.HashMap.Lazy
    (
      HashMap

      -- * Construction
    , HM.empty
    , singleton

      -- * Basic interface
    , HM.null
    , size
    , HM.lookup
    , lookupDefault
    , insert
    , HM.insertWith
    , delete
    , adjust

      -- * Combine
      -- ** Union
    , union
    , unionWith

      -- * Transformations
    , HM.map
    , traverseWithKey

      -- * Difference and intersection
    , difference
    , intersection

      -- * Folds
    , foldl'
    , foldlWithKey'
    , HM.foldr
    , foldrWithKey

      -- * Filter
    , HM.filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    ) where

import Control.Monad.ST (runST)
import Data.Hashable (Hashable)

import Data.HashMap.Base as HM
import Data.HashMap.Mutable as M

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = fromListWith const
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromList #-}
#endif

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f kvs0 = runST (go kvs0 M.empty >>= M.unsafeFreeze)
  where
    go [] !m = return m
    go ((k, v):kvs) m = do m' <- M.insertWith f k v m
                           go kvs m'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINE fromListWith #-}
#endif
