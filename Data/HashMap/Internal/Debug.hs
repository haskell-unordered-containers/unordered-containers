{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

-- | = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- Debugging utilities for 'HashMap's.

module Data.HashMap.Internal.Debug
    ( valid
    , Validity(..)
    , Error(..)
    , SubHash
    , SubHashPath
    ) where

import Data.Bits             (complement, countTrailingZeros, popCount,
                              unsafeShiftL, unsafeShiftR, (.&.))
import Data.Hashable         (Hashable)
import Data.HashMap.Internal (Bitmap, Hash, HashMap (..), Leaf (..),
                              bitsPerSubkey, fullBitmap, hash,
                              isLeafOrCollision, maxChildren, sparseIndex,
                              subkeyMask)
import Data.Semigroup        (Sum (..))

import qualified Data.HashMap.Internal.Array as A

data Validity k = Invalid (Error k) SubHashPath | Valid
  deriving (Eq, Show)

instance Semigroup (Validity k) where
  Valid <> y = y
  x     <> _ = x

instance Monoid (Validity k) where
  mempty = Valid
  mappend = (<>)

data Error k
  = INV1_internal_Empty
  | INV2_misplaced_hash !Hash
  | INV3_key_hash_mismatch k !Hash
  | INV4_Collision_size !Int
  | INV5_Collision_duplicate_key k !Hash
  | INV6_bad_BitmapIndexed_size !Int
  | INV7_bitmap_array_size_mismatch !Bitmap !Int
  | INV8_BitmapIndexed_invalid_single_subtree
  | INV9_bad_Full_size !Int
  deriving (Eq, Show)

-- | A part of a 'Hash' with 'bitsPerSubkey' bits.
type SubHash = Word

type SubHashPath = [SubHash]

valid :: Hashable k => HashMap k v -> Validity k
valid Empty = Valid
valid t     = validInternal [] t
  where
    validInternal p Empty                 = Invalid INV1_internal_Empty p
    validInternal p (Leaf h l)            = validHash p h <> validLeaf p h l
    validInternal p (Collision h ary)     = validHash p h <> validCollision p h ary
    validInternal p (BitmapIndexed b ary) = validBitmapIndexed p b ary
    validInternal p (Full ary)            = validFull p ary

    validHash p0 h0 = go (reverse p0) h0
      where
        go [] !_ = Valid
        go (sh:p) h | h .&. subkeyMask == sh = go p (h `unsafeShiftR` bitsPerSubkey)
                    | otherwise              = Invalid (INV2_misplaced_hash h0) p0

    validLeaf p h (L k _) | hash k == h = Valid
                          | otherwise   = Invalid (INV3_key_hash_mismatch k h) p

    validCollision p h ary = validCollisionSize <> A.foldMap (validLeaf p h) ary <> distinctKeys
      where
        n = A.length ary
        validCollisionSize | n < 2     = Invalid (INV4_Collision_size n) p
                           | otherwise = Valid
        distinctKeys = A.foldMap (\(L k _) -> appearsOnce k) ary
        appearsOnce k | A.foldMap (\(L k' _) -> if k' == k then Sum @Int 1 else Sum 0) ary == 1 = Valid
                      | otherwise = Invalid (INV5_Collision_duplicate_key k h) p

    validBitmapIndexed p b ary = validArraySize <> validSubTrees p b ary
      where
        n = A.length ary
        validArraySize | n < 1 || n >= maxChildren = Invalid (INV6_bad_BitmapIndexed_size n) p
                       | popCount b == n           = Valid
                       | otherwise                 = Invalid (INV7_bitmap_array_size_mismatch b n) p

    validSubTrees p b ary
      | A.length ary == 1
      , isLeafOrCollision (A.index ary 0)
      = Invalid INV8_BitmapIndexed_invalid_single_subtree p
      | otherwise = go b
      where
        go 0  = Valid
        go b' = validInternal (fromIntegral c : p) (A.index ary i) <> go b''
          where
            c = countTrailingZeros b'
            m = 1 `unsafeShiftL` c
            i = sparseIndex b m
            b'' = b' .&. complement m

    validFull p ary = validArraySize <> validSubTrees p fullBitmap ary
      where
        n = A.length ary
        validArraySize | n == maxChildren = Valid
                       | otherwise        = Invalid (INV9_bad_Full_size n) p
