{-# LANGUAGE BangPatterns, CPP #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap
-- Copyright   :  2010 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Maps from keys to values.  The implementation is based on
-- /big-endian patricia trees/ which are keyed by the hash value of
-- the original key.  A 'HashMap' is often faster than an tree-based
-- ordered map when key comparison is expensive, as in the case of
-- strings.
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).

module Data.HashMap
    (
      HashMap

      -- * Basic interface
    , size
    , lookup
    , empty
    , insert
    , delete
    , toList
    ) where

#include "MachDeps.h"

import Control.DeepSeq (NFData(rnf))
import Data.Bits ((.&.), (.|.), complement, shiftR, xor)
import Data.Hashable (Hashable(hash))
import qualified Data.FullList as FL
import Data.Word (Word)
import Prelude hiding (lookup)

------------------------------------------------------------------------
-- * The 'HashMap' type

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Nil
    | Tip {-# UNPACK #-} !Hash
          {-# UNPACK #-} !(FL.FullList k v)
    | Bin {-# UNPACK #-} !Prefix
          {-# UNPACK #-} !Mask
          !(HashMap k v)
          !(HashMap k v)
    deriving Show

type Prefix = Int
type Mask   = Int
type Hash   = Int

------------------------------------------------------------------------
-- * Instances

instance (Eq k, Eq v) => Eq (HashMap k v) where
    t1 == t2 = equal t1 t2
    t1 /= t2 = nequal t1 t2

equal :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2) =
    (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip h1 l1) (Tip h2 l2) = (h1 == h2) && (l1 == l2)
equal Nil Nil = True
equal _   _   = False

nequal :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2) =
    (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip h1 l1) (Tip h2 l2) = (h1 /= h2) || (l1 /= l2)
nequal Nil Nil = False
nequal _   _   = True

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Nil           = ()
    rnf (Tip _ xs)    = rnf xs
    rnf (Bin _ _ l r) = rnf l `seq` rnf r `seq` ()

------------------------------------------------------------------------
-- * Basic interface

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: HashMap k v -> Int
size t = case t of
    Bin _ _ l r -> size l + size r
    Tip _ l     -> FL.size l
    Nil         -> 0

-- | /O(min(n,W))/ Return the value to which the specified key is
-- mapped, or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k0 t = go h0 k0 t
  where
    h0 = hash k0
    go !h !k (Bin _ m l r)
      | zero h m  = go h k l
      | otherwise = go h k r
    go h k (Tip h' l)
      | h == h'   = FL.lookup k l
      | otherwise = Nothing
    go _ _ Nil    = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

-- | /O(1)/ Construct an empty 'HashMap'.
empty :: HashMap k v
empty = Nil

-- | /O(min(n,W))/ Associate the specified value with the specified
-- key in this map.  If the map previously contained a mapping for the
-- key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k0 v0 t0 = go h0 k0 v0 t0
  where
    h0 = hash k0
    go !h !k v t@(Bin p m l r)
        | nomatch h p m = join h (Tip h $ FL.singleton k v) p t
        | zero h m      = Bin p m (go h k v l) r
        | otherwise     = Bin p m l (go h k v r)
    go h k v t@(Tip h' l)
        | h == h'       = Tip h $ FL.insert k v l
        | otherwise     = join h (Tip h $ FL.singleton k v) h' t
    go h k v Nil        = Tip h $ FL.singleton k v
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(min(n,W))/ Remove the mapping for the specified key from this
-- map if present.
delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k0 = go h0 k0
  where
    h0 = hash k0
    go !h !k t@(Bin p m l r)
        | nomatch h p m = t
        | zero h m      = bin p m (go h k l) r  -- takes this branch
        | otherwise     = bin p m l (go h k r)
    go h k t@(Tip h' l)
        | h == h'       = case FL.delete k l of
            Nothing -> Nil
            Just l' -> Tip h' l'
        | otherwise     = t
    go _ _ Nil          = Nil
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

-- | /O(n)/ Convert the map to a list of key-value pairs.  The list is
-- generated lazily.
toList :: HashMap k v -> [(k, v)]
toList = fold (\k v xs -> (k, v) : xs) []

-- | /O(n)/ 'fold', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'HashMap',
-- reduces the 'HashMap' using the binary operator.
fold :: (k -> v -> a -> a) -> a -> HashMap k v -> a
fold f = go
  where
    go z (Bin _ _ l r) = go (go z r) l
    go z (Tip _ l)     = FL.fold f z l
    go z Nil           = z
{-# INLINE fold #-}

------------------------------------------------------------------------
-- Helpers

join :: Prefix -> HashMap k v -> Prefix -> HashMap k v -> HashMap k v
join p1 t1 p2 t2
    | zero p1 m = Bin p m t1 t2
    | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

-- | @bin@ assures that we never have empty trees within a tree.
bin :: Prefix -> Mask -> HashMap k v -> HashMap k v -> HashMap k v
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

------------------------------------------------------------------------
-- Endian independent bit twiddling

zero :: Hash -> Mask -> Bool
zero i m = (fromIntegral i :: Word) .&. (fromIntegral m :: Word) == 0
{-# INLINE zero #-}

nomatch :: Hash -> Prefix -> Mask -> Bool
nomatch i p m = (mask i m) /= p
{-# INLINE nomatch #-}

mask :: Hash -> Mask -> Prefix
mask i m = maskW (fromIntegral i :: Word) (fromIntegral m :: Word)
{-# INLINE mask #-}

------------------------------------------------------------------------
-- Big endian operations

maskW :: Word -> Word -> Prefix
maskW i m = fromIntegral (i .&. (complement (m-1) `xor` m))
{-# INLINE maskW #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2 =
    fromIntegral (highBit (fromIntegral p1 `xor` fromIntegral p2 :: Word))
{-# INLINE branchMask #-}

-- | Return a 'Word' where only the highest bit is set.
highBit :: Word -> Word
highBit x0 =
    let !x1 = x0 .|. shiftR x0 1
        !x2 = x1 .|. shiftR x1 2
        !x3 = x2 .|. shiftR x2 4
        !x4 = x3 .|. shiftR x3 8
        !x5 = x4 .|. shiftR x4 16
#if WORD_SIZE_IN_BITS == 32
    in x5 `xor` (shiftR x5 1)
#elif WORD_SIZE_IN_BITS == 64
        !x6 = x5 .|. shiftR x5 32
    in x6 `xor` (shiftR x6 1)
#else
# error WORD_SIZE_IN_BITS not supported
#endif
{-# INLINE highBit #-}
