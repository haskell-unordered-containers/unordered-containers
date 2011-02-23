{-# LANGUAGE BangPatterns, CPP #-}

-- | Code shared between the lazy and strict versions.

module Data.HashMap.Common
    (
      -- * Types
      HashMap(..)
    , size
    , root
    , sameSize
    , Tree(..)
    , Prefix
    , Mask
    , Hash

      -- * Helpers
    , join
    , bin
    , zero
    , nomatch
    , mask
    , maskW
    , branchMask
    , highBit

    -- * Common operations
    , traverseWithKey
    ) where

#include "MachDeps.h"

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Data.Bits ((.&.), (.|.), complement, shiftR, xor)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable(..))
import Data.Word (Word)
import Prelude hiding (foldr, map)

import qualified Data.FullList.Lazy as FL

------------------------------------------------------------------------
-- * The 'HashMap' type

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v = HM {-# UNPACK #-} !Int !(Tree k v)

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: HashMap k v -> Int
size (HM sz _) = sz
{-# INLINE size #-}

root :: HashMap k v -> Tree k v
root (HM _ t) = t

-- | Takes a function that operates 'Tree's, without changing their
-- size, and lifts it to operate on 'HashMap's.
sameSize :: (Tree k v -> Tree k v) -> HashMap k v -> HashMap k v
sameSize f hm = HM (size hm) $ f (root hm)
{-# INLINE sameSize #-}

data Tree k v
    = Nil
    | Tip {-# UNPACK #-} !Hash
          {-# UNPACK #-} !(FL.FullList k v)
    | Bin {-# UNPACK #-} !Prefix
          {-# UNPACK #-} !Mask
          !(Tree k v)
          !(Tree k v)
    deriving Show

type Prefix = Int
type Mask   = Int
type Hash   = Int

------------------------------------------------------------------------
-- * Instances

-- Since both the lazy and the strict API shares one data type we can
-- only provide one set of instances.  We provide the lazy ones.

instance (Eq k, Eq v) => Eq (HashMap k v) where
    t1 == t2 = size t1 == size t2 && equal (root t1) (root t2)
    t1 /= t2 = size t1 /= size t2 || nequal (root t1) (root t2)

equal :: (Eq k, Eq v) => Tree k v -> Tree k v -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2) =
    (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip h1 l1) (Tip h2 l2) = (h1 == h2) && (l1 == l2)
equal Nil Nil = True
equal _   _   = False

nequal :: (Eq k, Eq v) => Tree k v -> Tree k v -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2) =
    (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip h1 l1) (Tip h2 l2) = (h1 /= h2) || (l1 /= l2)
nequal Nil Nil = False
nequal _   _   = True

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf = go . root
      where
        go Nil           = ()
        go (Tip _ xs)    = rnf xs
        go (Bin _ _ l r) = go l `seq` go r `seq` ()

instance Functor (HashMap k) where
    fmap = map

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f hm = HM (size hm) $ go $ root hm
  where
    go (Bin p m l r) = Bin p m (go l) (go r)
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
foldrWithKey f z0 = go z0 . root
  where
    go z (Bin _ _ l r) = go (go z r) l
    go z (Tip _ l)     = FL.foldrWithKey f z l
    go z Nil           = z
{-# INLINE foldrWithKey #-}

instance Traversable (HashMap k) where
  traverse f = traverseWithKey (const f)

-- | /O(n)/ Transform this map by accumulating an Applicative result from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HashMap k v1
                -> f (HashMap k v2)
traverseWithKey f hm = HM (size hm) <$> (go $ root hm)
  where
    go (Bin p m l r) = Bin p m <$> go l <*> go r
    go (Tip h l) = Tip h <$> FL.traverseWithKey f l
    go Nil = pure Nil
{-# INLINE traverseWithKey #-}

------------------------------------------------------------------------
-- Helpers

join :: Prefix -> Tree k v -> Prefix -> Tree k v -> Tree k v
join p1 t1 p2 t2
    | zero p1 m = Bin p m t1 t2
    | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE join #-}

-- | @bin@ assures that we never have empty trees within a tree.
bin :: Prefix -> Mask -> Tree k v -> Tree k v -> Tree k v
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
