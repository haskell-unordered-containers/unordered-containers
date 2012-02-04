{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- | Code shared between the lazy and strict versions.

module Data.HashMap.Common
    (
      -- * Types
      HashMap(..), SuffixMask, Hash

      -- * Helpers
    , join
    , bin
    , zero
    , nomatch

    -- * Construction
    , empty

    -- * Combine
    , union

    -- * Transformations
    , toList
    , filterMapWithKey
    , traverseWithKey

    -- * Folds
    , foldrWithKey

    -- * Helpers
    , shorter
    , insertCollidingWith
    ) where

#include "MachDeps.h"

import Control.Applicative (Applicative((<*>), pure), (<$>))
import Control.DeepSeq (NFData(rnf))
import Data.Bits (Bits(..), (.&.), xor)
import qualified Data.Foldable as Foldable
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import Data.Word (Word)
import Prelude hiding (foldr, map)

#if defined(__GLASGOW_HASKELL__)
import GHC.Exts (build)
#endif

import qualified Data.FullList.Lazy as FL

------------------------------------------------------------------------
-- * The 'HashMap' type

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Bin {-# UNPACK #-} !SuffixMask
          !(HashMap k v)
          !(HashMap k v)
    | Tip {-# UNPACK #-} !Hash
          {-# UNPACK #-} !(FL.FullList k v)
    | Empty
    deriving (Typeable)

type Suffix = Int
type Hash   = Int

-- | A SuffixMask stores a path to a Bin node in the hash map.  The
-- uppermost set bit, the Mask, indicates the bit used to distinguish
-- hashes in the left and right subtrees.  The lower-order bits (below
-- the highest set bit), the Suffix, are set the same way in all the
-- hashes contained in this subtree of the map.  Thus, hashes in the
-- right subtree will match all the bits in the SuffixMask, but may
-- have set bits above the Mask.  Hashes in the left subtree will not
-- match the Mask bit, but will match all the Suffix bits.
type SuffixMask = Int

------------------------------------------------------------------------
-- * Instances

-- Since both the lazy and the strict API shares one data type we can
-- only provide one set of instances.  We provide the lazy ones.

instance (Eq k, Eq v) => Eq (HashMap k v) where
    t1 == t2 = equal t1 t2
    t1 /= t2 = nequal t1 t2

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily.
toList :: HashMap k v -> [(k, v)]
#if defined(__GLASGOW_HASKELL__)
toList t = build (\ c z -> foldrWithKey (curry c) z t)
#else
toList = foldrWithKey (\ k v xs -> (k, v) : xs) []
#endif
{-# INLINE toList #-}

equal :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
equal (Bin sm1 l1 r1) (Bin sm2 l2 r2) =
    (sm1 == sm2) && (equal l1 l2) && (equal r1 r2)
equal (Tip h1 l1) (Tip h2 l2) = (h1 == h2) && (l1 == l2)
equal Empty Empty = True
equal _     _     = False

nequal :: (Eq k, Eq v) => HashMap k v -> HashMap k v -> Bool
nequal (Bin sm1 l1 r1) (Bin sm2 l2 r2) =
    (sm1 /= sm2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip h1 l1) (Tip h2 l2) = (h1 /= h2) || (l1 /= l2)
nequal Empty Empty = False
nequal _     _     = True

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty       = ()
    rnf (Tip _ xs)  = rnf xs
    rnf (Bin _ l r) = rnf l `seq` rnf r

instance Functor (HashMap k) where
    fmap = map

instance (Show k, Show v) => Show (HashMap k v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toList m)

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go (Bin sm l r) = Bin sm (go l) (go r)
    go (Tip h l)    = Tip h (FL.map f' l)
    go Empty        = Empty
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
    go z (Bin _ l r) = go (go z r) l
    go z (Tip _ l)   = FL.foldrWithKey f z l
    go z Empty       = z
{-# INLINE foldrWithKey #-}

instance Eq k => Monoid (HashMap k v) where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = union
  {-# INLINE mappend #-}

-- | /O(1)/ Construct an empty map.
empty :: HashMap k v
empty = Empty

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union :: Eq k => HashMap k v -> HashMap k v -> HashMap k v
union t1@(Bin sm1 l1 r1) t2@(Bin sm2 l2 r2)
    | sm1 == sm2      = Bin sm1 (union l1 l2) (union r1 r2)
    | shorter sm1 sm2 = union1
    | shorter sm2 sm1 = union2
    | otherwise       = join sm1 t1 sm2 t2
  where
    union1 | nomatch sm2 sm1 = join sm1 t1 sm2 t2
           | zero sm2 sm1    = Bin sm1 (union l1 t2) r1
           | otherwise       = Bin sm1 l1 (union r1 t2)

    union2 | nomatch sm1 sm2 = join sm1 t1 sm2 t2
           | zero sm1 sm2    = Bin sm2 (union t1 l2) r2
           | otherwise       = Bin sm2 l2 (union t1 r2)
union (Tip h l) t = insertCollidingL h l t
union t (Tip h l) = insertCollidingR h l t  -- right bias
union Empty t     = t
union t Empty     = t
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
    go !h !xs t@(Bin sm l r)
        | nomatch h sm = join h (Tip h xs) sm t
        | zero h sm    = Bin sm (go h xs l) r
        | otherwise    = Bin sm l (go h xs r)
    go h xs t@(Tip h' l)
        | h == h'       = Tip h $ f xs l
        | otherwise     = join h (Tip h xs) h' t
    go h xs Empty       = Tip h xs
{-# INLINE insertCollidingWith #-}

instance Traversable (HashMap k) where
  traverse f = traverseWithKey (const f)

-- | /O(n)/ Transform this map by applying a function to every value;
-- when f k v returns Just x, keep an entry mapping k to x, otherwise
-- do not include k in the result.
filterMapWithKey :: (k -> v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
filterMapWithKey f = go
  where
    go (Bin sm l r) = bin sm (go l) (go r)
    go (Tip h vs) =
      case FL.foldrWithKey ff FL.Nil vs of
        FL.Nil -> Empty
        FL.Cons k v xs -> Tip h (FL.FL k v xs)
    go Empty = Empty
    ff k v xs =
      case f k v of
        Nothing -> xs
        Just x  -> FL.Cons k x xs
{-# INLINE filterMapWithKey #-}

-- | /O(n)/ Transform this map by accumulating an Applicative result
-- from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HashMap k v1
                -> f (HashMap k v2)
traverseWithKey f = go
  where
    go (Bin sm l r) = Bin sm <$> go l <*> go r
    go (Tip h l) = Tip h <$> FL.traverseWithKey f l
    go Empty = pure Empty
{-# INLINE traverseWithKey #-}

------------------------------------------------------------------------
-- Helpers

join :: Suffix -> HashMap k v -> Suffix -> HashMap k v -> HashMap k v
join s1 t1 s2 t2
    | zero s1 sm = Bin sm t1 t2
    | otherwise  = Bin sm t2 t1
  where
    sm = branchSuffixMask s1 s2
{-# INLINE join #-}

-- | @bin@ assures that we never have empty trees within a tree.
bin :: SuffixMask -> HashMap k v -> HashMap k v -> HashMap k v
bin _ l Empty = l
bin _ Empty r = r
bin sm  l r = Bin sm l r
{-# INLINE bin #-}

------------------------------------------------------------------------
-- Endian independent bit twiddling

-- Actually detects if every set bit of sm is set in i (and returns
-- false if so).  In most cases, the Suffix will already match, and
-- this just tests the Mask.  For lookup it can send us down the wrong
-- path, but that's OK; we'll detect this when we reach a Tip and
-- don't match.  We could have checked (i .|. fromIntegral sm) /= i
-- instead.
zero :: Hash -> SuffixMask -> Bool
zero i sm = (i .&. smi) /= smi
  where smi = fromIntegral sm
{-# INLINE zero #-}

-- We want to detect Suffix bits in the Hash that differ from
-- SuffixMask.  To do this, we find the first bit that differs between
-- Hash and SuffixMask, then check if that bit is smaller than the
-- Mask bit.  We do this by observing that if we set this bit and all
-- bits to its right, we'll obtain a number >= the suffixmask if all
-- bits are the same (cb == 0, setting all bits) or if the first bit of
-- difference is >= the Mask.  Note: this comparison must be unsigned.
nomatch :: Hash -> SuffixMask -> Bool
nomatch i sm = (cb + cb - 1) < fromIntegral sm
  where cb = differentBit i (fromIntegral sm)
{-# INLINE nomatch #-}

------------------------------------------------------------------------
-- Big endian operations

-- | Compute the first (lowest-order) bit at which h1 and h2 differ.
-- This is the mask that distinguishes them.
differentBit :: Hash -> Hash -> Word
differentBit h1 h2 =
  fromIntegral (critBit (fromIntegral h1 `xor` fromIntegral h2))

-- | Given mask bit m expressed as a word, compute the suffix bits of
-- hash i, also expressed as a word.
suffixW :: Word -> Word -> Word
suffixW i m = i .&. (m-1)
{-# INLINE suffixW #-}

-- | Given two hashes and/or SuffixMasks for which nomatch p1 p2 &&
-- nomatch p2 p1, compute SuffixMask that differentiates them, by
-- first computing the mask m and then using that to derive a suffix
-- from one of them (it won't matter which, as those bits are the
-- same).
branchSuffixMask :: Suffix -> Suffix -> SuffixMask
branchSuffixMask p1 p2 =
    fromIntegral (m + suffixW w1 m)
  where m = differentBit p1 p2
        w1 = fromIntegral p1
{-# INLINE branchSuffixMask #-}

-- | Is the mask of sm1 closer to the root of the tree (lower order)
-- than the mask of sm2?  This is actually approximate, and returns
-- junk when both sm1 and sm2 are at the same tree level.  This must
-- be disambiguated by first checking sm1==sm2, and subsequently by
-- checking nomatch in the appropriate direction (which will need to
-- happen anyway to determine if insertion or branching is
-- appropriate).
shorter :: SuffixMask -> SuffixMask -> Bool
shorter sm1 sm2 = (fromIntegral sm1 :: Word) < (fromIntegral sm2 :: Word)
{-# INLINE shorter #-}

-- | Return a 'Word' whose single set bit corresponds to the lowest set bit of w.
critBit :: Word -> Word
critBit w = w .&. (negate w)
{-# INLINE critBit #-}
