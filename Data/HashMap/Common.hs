{-# LANGUAGE BangPatterns, CPP #-}

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
    , maskW
    , branchMask
    , critBit

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
data HashMap k v
    = Nil
    | Tip {-# UNPACK #-} !Hash
          {-# UNPACK #-} !(FL.FullList k v)
    | Bin {-# UNPACK #-} !Suffix
          {-# UNPACK #-} !Mask
          !(HashMap k v)
          !(HashMap k v)
    deriving Show

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
    rnf (Bin _ _ l r) = rnf l `seq` rnf r `seq` ()

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

instance Traversable (HashMap k) where
  traverse f = traverseWithKey (const f)

-- | /O(n)/ Transform this map by accumulating an Applicative result from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HashMap k v1 -> f (HashMap k v2)
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

branchMask :: Suffix -> Suffix -> Mask
branchMask p1 p2 =
    fromIntegral (critBit (fromIntegral p1 `xor` fromIntegral p2 :: Word))
{-# INLINE branchMask #-}

-- | Return a 'Word' where only the highest bit is set.
critBit :: Word -> Word
critBit w = w' `xor` (w' `shiftR` 1) where
    w' = w `xor` (w-1)
{-# INLINE critBit #-}
