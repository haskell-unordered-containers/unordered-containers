{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable #-}

-- | Code shared between the lazy and strict versions.

module Data.HashMap.Common
    (
      -- * Types
      HashMap(..)
    , Suffix
    , Mask
    , Hash

      -- * Helpers
    , join
    , bin
    , zero
    , nomatch
    , mask

    -- * Construction
    , empty

    -- * Combine
    , union

    -- * Transformations
    , traverseWithKey

    -- * Folds
    , foldrWithKey
    ) where

#include "MachDeps.h"

import Control.Applicative (Applicative((<*>), pure), (<$>))
import Control.DeepSeq (NFData(rnf))
import Data.Bits ((.&.), xor)
import qualified Data.Foldable as Foldable
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import Data.Word (Word)
import Prelude hiding (foldr, map)

import qualified Data.FullList.Lazy as FL

------------------------------------------------------------------------
-- * The 'HashMap' type

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Nil
    | Tip {-# UNPACK #-} !Hash
          {-# UNPACK #-} !(FL.FullList k v)
    | Bin {-# UNPACK #-} !Suffix
          {-# UNPACK #-} !Mask
          !(HashMap k v)
          !(HashMap k v)
    deriving (Show, Typeable)

type Suffix = Int
type Mask   = Int
type Hash   = Int

------------------------------------------------------------------------
-- * Instances

-- Since both the lazy and the strict API shares one data type we can
-- only provide one set of instances.  We provide the lazy ones.

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
    rnf (Bin _ _ l r) = rnf l `seq` rnf r

instance Functor (HashMap k) where
    fmap = map

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go (Bin s m l r) = Bin s m (go l) (go r)
    go (Tip h l)     = Tip h (FL.map f' l)
    go Nil           = Nil
    f' k v = (k, f v)
{-# INLINE map #-}

instance Foldable.Foldable (HashMap k) where
    foldr f = foldrWithKey (const f)

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f = go
  where
    go z (Bin _ _ l r) = go (go z r) l
    go z (Tip _ l)     = FL.foldrWithKey f z l
    go z Nil           = z
{-# INLINE foldrWithKey #-}

instance Eq k => Monoid (HashMap k v) where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = union
  {-# INLINE mappend #-}

-- | /O(1)/ Construct an empty map.
empty :: HashMap k v
empty = Nil

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union :: Eq k => HashMap k v -> HashMap k v -> HashMap k v
union t1@(Bin s1 m1 l1 r1) t2@(Bin s2 m2 l2 r2)
    | shorter m1 m2 = union1
    | shorter m2 m1 = union2
    | s1 == s2      = Bin s1 m1 (union l1 l2) (union r1 r2)
    | otherwise     = join s1 t1 s2 t2
  where
    union1 | nomatch s2 s1 m1 = join s1 t1 s2 t2
           | zero s2 m1       = Bin s1 m1 (union l1 t2) r1
           | otherwise        = Bin s1 m1 l1 (union r1 t2)

    union2 | nomatch s1 s2 m2 = join s1 t1 s2 t2
           | zero s1 m2       = Bin s2 m2 (union t1 l2) r2
           | otherwise        = Bin s2 m2 l2 (union t1 r2)
union (Tip h l) t = insertCollidingL h l t
union t (Tip h l) = insertCollidingR h l t  -- right bias
union Nil t       = t
union t Nil       = t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE union #-}
#endif

-- | Insert a list of key-value pairs which keys all hash to the same
-- hash value.  Prefer key-value pairs in the list to key-value pairs
-- already in the map.
insertCollidingL :: Eq k => Hash -> FL.FullList k v -> HashMap k v -> HashMap k v
insertCollidingL = insertCollidingWith FL.union
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertCollidingL #-}
#endif

-- | Insert a list of key-value pairs which keys all hash to the same
-- hash value.  Prefer key-value pairs already in the map to key-value
-- pairs in the list.
insertCollidingR :: Eq k => Hash -> FL.FullList k v -> HashMap k v -> HashMap k v
insertCollidingR = insertCollidingWith (flip FL.union)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertCollidingR #-}
#endif

-- | Insert a list of key-value pairs which keys all hash to the same
-- hash value.  Merge the list of key-value pairs to be inserted @xs@
-- with any existing key-values pairs @ys@ by applying @f xs ys@.
insertCollidingWith :: Eq k
                    => (FL.FullList k v -> FL.FullList k v -> FL.FullList k v)
                    -> Hash -> FL.FullList k v
                    -> HashMap k v -> HashMap k v
insertCollidingWith f h0 l0 t0 = go h0 l0 t0
  where
    go !h !xs t@(Bin s m l r)
        | nomatch h s m = join h (Tip h xs) s t
        | zero h m      = Bin s m (go h xs l) r
        | otherwise     = Bin s m l (go h xs r)
    go h xs t@(Tip h' l)
        | h == h'       = Tip h $ f xs l
        | otherwise     = join h (Tip h xs) h' t
    go h xs Nil         = Tip h xs
{-# INLINE insertCollidingWith #-}

instance Traversable (HashMap k) where
  traverse f = traverseWithKey (const f)

-- | /O(n)/ Transform this map by accumulating an Applicative result
-- from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HashMap k v1
                -> f (HashMap k v2)
traverseWithKey f = go
  where
    go (Bin p m l r) = Bin p m <$> go l <*> go r
    go (Tip h l) = Tip h <$> FL.traverseWithKey f l
    go Nil = pure Nil
{-# INLINE traverseWithKey #-}

------------------------------------------------------------------------
-- Helpers

join :: Suffix -> HashMap k v -> Suffix -> HashMap k v -> HashMap k v
join s1 t1 s2 t2
    | zero s1 m = Bin s m t1 t2
    | otherwise = Bin s m t2 t1
  where
    m = branchMask s1 s2
    s = mask s1 m
{-# INLINE join #-}

-- | @bin@ assures that we never have empty trees within a tree.
bin :: Suffix -> Mask -> HashMap k v -> HashMap k v -> HashMap k v
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

------------------------------------------------------------------------
-- Endian independent bit twiddling

zero :: Hash -> Mask -> Bool
zero i m = (fromIntegral i :: Word) .&. (fromIntegral m :: Word) == 0
{-# INLINE zero #-}

nomatch :: Hash -> Suffix -> Mask -> Bool
nomatch i s m = (mask i m) /= s
{-# INLINE nomatch #-}

mask :: Hash -> Mask -> Suffix
mask i m = maskW (fromIntegral i :: Word) (fromIntegral m :: Word)
{-# INLINE mask #-}

------------------------------------------------------------------------
-- Big endian operations

maskW :: Word -> Word -> Suffix
maskW i m = fromIntegral (i .&. (m-1))
{-# INLINE maskW #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2 = (fromIntegral m1 :: Word) < (fromIntegral m2 :: Word)
{-# INLINE shorter #-}

branchMask :: Suffix -> Suffix -> Mask
branchMask p1 p2 =
    fromIntegral (critBit (fromIntegral p1 `xor` fromIntegral p2 :: Word))
{-# INLINE branchMask #-}

-- | Return a 'Word' whose single set bit corresponds to the lowest set bit of w.
critBit :: Word -> Word
critBit w = w .&. (negate w)
{-# INLINE critBit #-}
