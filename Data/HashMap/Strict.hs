{-# LANGUAGE BangPatterns, CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Strict
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
-- This map is strict in both the keys and the values; keys and values
-- are evaluated to /weak head normal form/ before they are added to
-- the map.  Exception: the provided instances are the same as for the
-- lazy version of this module.
--
-- The implementation is based on /hash array mapped trie/.  A
-- 'HashMap' is often faster than other tree-based set types,
-- especially when key comparison is expensive, as in the case of
-- strings.
--
-- Many operations have a average-case complexity of /O(log n)/.  The
-- implementation uses a large base (i.e. 16) so in practice these
-- operations are constant time.
module Data.HashMap.Strict
    (
      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , HM.null
    , size
    , HM.lookup
    , lookupDefault
    , insert
    , insertWith
    , delete
    , adjust

      -- * Combine
      -- ** Union
    , union
    , unionWith

      -- * Transformations
    , map
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

import Data.Bits ((.&.), (.|.))
import qualified Data.List as L
import Data.Hashable (Hashable)
import Prelude hiding (map)

import qualified Data.HashMap.Array as A
import qualified Data.HashMap.Base as HM
import Data.HashMap.Base hiding (
    adjust, fromList, fromListWith, insert, insertWith, map, singleton,
    unionWith)

------------------------------------------------------------------------
-- * Construction

-- | /O(1)/ Construct a map with a single element.
singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k !v = HM.singleton k v

------------------------------------------------------------------------
-- * Basic interface

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k !v = HM.insert k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
           -> HashMap k v
insertWith = insertWith'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

-- Always inlined to the implementation of 'insert' doesn't have to
-- pay the cost of using the higher-order argument.
insertWith' :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
            -> HashMap k v
insertWith' f k0 !v0 = go h0 k0 v0 0
  where
    h0 = hash k0
    go !h !k x !_ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then let !v' = f x y in Leaf h (L k v')
                    else collision h l (L k x)
        -- TODO: Could we avoid creating a one element array that will
        -- most likely be reallocated as a two element array?
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
    go h k x s (BitmapIndexed b ary)
        | b .&. m == 0 = let l    = Leaf h (L k x)
                             ary' = A.insert ary i $! l
                             b'   = b .|. m
                         in if b' == fullNodeMask
                            then Full ary'
                            else BitmapIndexed b' ary'
        | otherwise = let st   = A.index ary i
                          st'  = go h k x (s+bitsPerSubkey) st
                          ary' = A.update ary i $! st'
                      in BitmapIndexed b ary'
      where m = bitpos h s
            i = index b m
    go h k x s (Full ary) =
        let i    = mask h s
            st   = A.index ary i
            st'  = go h k x (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
    go h k x s t@(Collision hy v)
        | h == hy = Collision h (updateOrSnocWith f k x v)
        -- TODO: See above TODO on single element arrays.
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
{-# INLINE insertWith' #-}

-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky y))
        | hy == h && ky == k = let !v' = f y in Leaf h (L k v')
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise = let st   = A.index ary i
                          st'  = go h k (s+bitsPerSubkey) st
                          ary' = A.update ary i $! st'
                      in BitmapIndexed b ary'
      where m = bitpos h s
            i = index b m
    go h k s (Full ary) =
        let i    = mask h s
            st   = A.index ary i
            st'  = go h k (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
    go h k _ t@(Collision hy v)
        | h == hy   = Collision h (updateWith f k v)
        | otherwise = t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#endif

------------------------------------------------------------------------
-- * Combine

-- TODO: Change to use new union algorithm.
-- | /O(n*log m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWith f m1 m2 = foldlWithKey' (\ m k v -> insertWith f k v m) m2 m1
{-# INLINE unionWith #-}

------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go Empty                 = Empty
    go (Leaf h (L k v))      = let !v' = f v in Leaf h $ L k v'
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map' go ary
    go (Full ary)            = Full $ A.map' go ary
    go (Collision h ary)     =
        Collision h $ A.map' (\ (L k v) -> let !v' = f v in L k v') ary
{-# INLINE map #-}

-- TODO: Should we add a strict traverseWithKey?

------------------------------------------------------------------------
-- ** Lists

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = L.foldl' (\ m (k, v) -> insert k v m) empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromList #-}
#endif

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = L.foldl' (\ m (k, v) -> insertWith f k v m) empty
{-# INLINE fromListWith #-}

------------------------------------------------------------------------
-- Array operations

updateWith :: Eq k => (v -> v) -> k -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateWith f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = ary
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> let !v' = f y in A.update ary i (L k v')
                     | otherwise -> go k ary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateWith #-}
#endif

updateOrSnocWith :: Eq k => (v -> v -> v) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWith f k0 v0 ary0 = go k0 v0 ary0 0 (A.length ary0)
  where
    go !k v !ary !i !n
        | i >= n = A.run $ do
            -- Not found, append to the end.
            mary <- A.new_ (n + 1)
            A.copy ary 0 mary 0 n
            A.write mary n (L k v)
            return mary
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> let !v' = f v y in A.update ary i (L k v')
                     | otherwise -> go k v ary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateOrSnocWith #-}
#endif
