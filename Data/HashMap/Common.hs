{-# LANGUAGE BangPatterns, CPP #-}

-- | Code shared between the lazy and strict versions.

module Data.HashMap.Common
    (
      -- * Types
      HashMap(..)
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
    ) where

#include "MachDeps.h"

import Data.Bits ((.&.), (.|.), complement, shiftR, xor)
import Data.Word (Word)

import qualified Data.FullList.Lazy as FL

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
