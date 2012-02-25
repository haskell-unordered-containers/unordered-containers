{-# LANGUAGE BangPatterns, CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Lazy
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
-- This map is strict in the keys and lazy in the values; keys are
-- evaluated to /weak head normal form/ before they are added to the
-- map.
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

module Data.HashMap.Lazy
    (
      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , lookup
    , lookupDefault
    , insert
    , delete
    , insertWith
    , insertLookupWithKey
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
    , fromListWith
    ) where

import qualified Data.FullList.Lazy as FL
import Data.Hashable (Hashable(hash))
import qualified Data.List as List
import Prelude hiding (filter, foldr, lookup, map, null, pred)

import Data.HashMap.Common

------------------------------------------------------------------------
-- * Basic interface

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: HashMap k v -> Bool
null Empty = True
null _     = False

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: HashMap k v -> Int
size t = go t 0
  where
    go (Bin _ l r) !sz = go r (go l sz)
    go (Tip _ l)   !sz = sz + FL.size l
    go Empty       !sz = sz

-- | /O(min(n,W))/ Return the value to which the specified key is
-- mapped, or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k0 t = go h0 k0 t
  where
    h0 = hash k0
    go !h !k (Bin sm l r)
      | zero h sm = go h k l
      | otherwise = go h k r
    go h k (Tip h' l)
      | h == h'   = FL.lookup k l
      | otherwise = Nothing
    go _ _ Empty  = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

-- | /O(min(n,W))/ Return the value to which the specified key is
-- mapped, or the default value if this map contains no mapping for
-- the key.
lookupDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
lookupDefault def k t = case lookup k t of
                          Just v -> v
                          _      -> def
{-# INLINABLE lookupDefault #-}

-- | /O(1)/ Construct a map with a single element.
singleton :: Hashable k => k -> v -> HashMap k v
singleton k v = Tip h $ FL.singleton k v
  where h = hash k
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE singleton #-}
#endif

-- | /O(min(n,W))/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k0 v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin sm l r)
        | nomatch h sm = join h (Tip h $ FL.singleton k v) sm t
        | zero h sm    = Bin sm (go h k v l) r
        | otherwise    = Bin sm l (go h k v r)
    go h k v t@(Tip h' l)
        | h == h'      = Tip h $ FL.insert k v l
        | otherwise    = join h (Tip h $ FL.singleton k v) h' t
    go h k v Empty     = Tip h $ FL.singleton k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(min(n,W))/ Remove the mapping for the specified key from this
-- map if present.
delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k0 = go h0 k0
  where
    h0 = hash k0
    go !h !k t@(Bin sm l r)
        | nomatch h sm = t
        | zero h sm    = bin sm (go h k l) r
        | otherwise    = bin sm l (go h k r)
    go h k t@(Tip h' l)
        | h == h'       = case FL.delete k l of
            Nothing -> Empty
            Just l' -> Tip h' l'
        | otherwise     = t
    go _ _ Empty        = Empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
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
insertWith f k0 v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin sm l r)
        | nomatch h sm = join h (Tip h $ FL.singleton k v) sm t
        | zero h sm    = Bin sm (go h k v l) r
        | otherwise    = Bin sm l (go h k v r)
    go h k v t@(Tip h' l)
        | h == h'       = Tip h $ FL.insertWith f k v l
        | otherwise     = join h (Tip h $ FL.singleton k v) h' t
    go h k v Empty      = Tip h $ FL.singleton k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

-- | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWith' (f k) k x map@).
insertLookupWithKey :: (Eq k, Hashable k)
                    => (k -> v -> v -> v) -> k -> v -> HashMap k v
                    -> (Maybe v, HashMap k v)
insertLookupWithKey f k0 v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin sm l r)
        | nomatch h sm = (Nothing, join h (Tip h $ FL.singleton k v) sm t)
        | zero h sm    = let (found, l') = go h k v l in (found, Bin sm l' r)
        | otherwise    = let (found, r') = go h k v r in (found, Bin sm l  r')
    go h k v t@(Tip h' l)
        | h == h'       = let (found, fl) = FL.insertLookupWith (f k) k v l
                          in (found, Tip h fl)
        | otherwise     = (Nothing, join h (Tip h $ FL.singleton k v) h' t)
    go h k v Empty      = (Nothing, Tip h $ FL.singleton k v)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertLookupWithKey #-}
#endif

-- | /O(min(n,W)/ Adjust the value tied to a given key in this map
-- only if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k0 t0 = go h0 k0 t0
  where
    h0 = hash k0
    go !h !k t@(Bin sm l r)
      | nomatch h sm = t
      | zero h sm    = Bin sm (go h k l) r
      | otherwise    = Bin sm l (go h k r)
    go h k t@(Tip h' l)
      | h == h'      = Tip h $ FL.adjust f k l
      | otherwise    = t
    go _ _ Empty     = Empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#endif

------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go (Bin sm l r) = Bin sm (go l) (go r)
    go (Tip h l)    = Tip h (FL.map f' l)
    go Empty        = Empty
    f' k v = (k, f v)
{-# INLINE map #-}

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWith :: Eq k => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
unionWith f t1@(Bin sm1 l1 r1) t2@(Bin sm2 l2 r2)
    | sm1 == sm2      = Bin sm1 (unionWith f l1 l2) (unionWith f r1 r2)
    | shorter sm1 sm2 = union1
    | shorter sm2 sm1 = union2
    | otherwise       = join sm1 t1 sm2 t2
  where
    union1 | nomatch sm2 sm1 = join sm1 t1 sm2 t2
           | zero sm2 sm1    = Bin sm1 (unionWith f l1 t2) r1
           | otherwise       = Bin sm1 l1 (unionWith f r1 t2)

    union2 | nomatch sm1 sm2 = join sm1 t1 sm2 t2
           | zero sm1 sm2    = Bin sm2 (unionWith f t1 l2) r2
           | otherwise       = Bin sm2 l2 (unionWith f t1 r2)
unionWith f (Tip h l) t = insertCollidingWith (FL.unionWith f) h l t
unionWith f t (Tip h l) = insertCollidingWith (flip (FL.unionWith f)) h l t  -- right bias
unionWith _ Empty t     = t
unionWith _ t Empty     = t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionWith #-}
#endif

-- | /O(n)/ Difference of two maps. Return elements of the first map
-- not existing in the second.
difference :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> insert k v m
                 _       -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE difference #-}
#endif

-- | /O(n)/ Intersection of two maps. Return elements of the first map
-- for keys existing in the second.
intersection :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
intersection a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just _ -> insert k v m
                 _      -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersection #-}
#endif

------------------------------------------------------------------------
-- * Folds

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> HashMap k v -> a
foldr f = foldrWithKey (const f)
{-# INLINE foldr #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> HashMap k v -> a
foldl' f = foldlWithKey' (\ z _ v -> f z v)
{-# INLINE foldl' #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' f = go
  where
    go !z (Bin _ l r) = let z' = go z l
                        in z' `seq` go z' r
    go z (Tip _ l)    = FL.foldlWithKey' f z l
    go z Empty        = z
{-# INLINE foldlWithKey' #-}

------------------------------------------------------------------------
-- * Filter

-- | /O(n)/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: (k -> v -> Bool) -> HashMap k v -> HashMap k v
filterWithKey pred = go
  where
    go (Bin sm l r) = bin sm (go l) (go r)
    go (Tip h l)    = case FL.filterWithKey pred l of
        Just l' -> Tip h l'
        Nothing -> Empty
    go Empty        = Empty
{-# INLINE filterWithKey #-}

-- | /O(n)/ Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (v -> Bool) -> HashMap k v -> HashMap k v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

------------------------------------------------------------------------
-- Conversions

-- | /O(n*min(W, n))/ Construct a map from a list of elements.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = List.foldl' (\ m (k, v) -> insert k v m) empty
{-# INLINE fromList #-}

-- | /O(n*min(W, n))/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = List.foldl' (\ m (k, v) -> insertWith f k v m) empty
{-# INLINE fromListWith #-}

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: HashMap k v -> [k]
keys = List.map fst . toList
{-# INLINE keys #-}

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: HashMap k v -> [v]
elems = List.map snd . toList
{-# INLINE elems #-}
