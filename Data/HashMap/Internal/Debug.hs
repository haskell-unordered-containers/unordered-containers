{-# LANGUAGE CPP              #-}
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
                              unsafeShiftL, (.&.), (.|.))
import Data.Hashable         (Hashable)
import Data.HashMap.Internal (Bitmap, Hash, HashMap (..), Leaf (..),
                              bitsPerSubkey, fullBitmap, hash,
                              isLeafOrCollision, maxChildren, sparseIndex)
import Data.Semigroup        (Sum (..))

import qualified Data.HashMap.Internal.Array as A


#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

data Validity k = Invalid (Error k) SubHashPath | Valid
  deriving (Eq, Show)

instance Semigroup (Validity k) where
  Valid <> y = y
  x     <> _ = x

instance Monoid (Validity k) where
  mempty = Valid
  mappend = (<>)

-- | An error corresponding to a broken invariant.
--
-- See 'HashMap' for the documentation of the invariants.
data Error k
  = INV1_internal_Empty
  | INV2_bad_BitmapIndexed_size !Int
  | INV3_bitmap_array_size_mismatch !Bitmap !Int
  | INV4_BitmapIndexed_invalid_single_subtree
  | INV5_misplaced_hash !Hash
  | INV6_key_hash_mismatch k !Hash
  | INV7_bad_Full_size !Int
  | INV8_Collision_size !Int
  | INV9_Collision_duplicate_key k !Hash
  deriving (Eq, Show)

-- TODO: Name this 'Index'?!
-- (https://github.com/haskell-unordered-containers/unordered-containers/issues/425)
-- | A part of a 'Hash' with 'bitsPerSubkey' bits.
type SubHash = Word

data SubHashPath = SubHashPath
  { partialHash :: !Word
    -- ^ The bits we already know, starting from the lower bits.
    -- The unknown upper bits are @0@.
  , lengthInBits :: !Int
    -- ^ The number of bits known. @>= 0 && <= finiteBitSize @Word 0@
  } deriving (Eq, Show)

initialSubHashPath :: SubHashPath
initialSubHashPath = SubHashPath 0 0

addSubHash :: SubHashPath -> SubHash -> SubHashPath
addSubHash (SubHashPath ph l) sh =
  SubHashPath (ph .|. (sh `unsafeShiftL` l)) (l + bitsPerSubkey)

valid :: Hashable k => HashMap k v -> Validity k
valid Empty = Valid
valid t     = validInternal initialSubHashPath t
  where
    validInternal p Empty                 = Invalid INV1_internal_Empty p
    validInternal p (Leaf h l)            = validHash p h <> validLeaf p h l
    validInternal p (Collision h ary)     = validHash p h <> validCollision p h ary
    validInternal p (BitmapIndexed b ary) = validBitmapIndexed p b ary
    validInternal p (Full ary)            = validFull p ary

    validHash p@(SubHashPath ph l) h | maskToLength h l == ph = Valid
                                     | otherwise              = Invalid (INV5_misplaced_hash h) p
      where
        maskToLength h' l' = h' .&. complement (complement 0 `unsafeShiftL` l')

    validLeaf p h (L k _) | hash k == h = Valid
                          | otherwise   = Invalid (INV6_key_hash_mismatch k h) p

    validCollision p h ary = validCollisionSize <> A.foldMap (validLeaf p h) ary <> distinctKeys
      where
        n = A.length ary
        validCollisionSize | n < 2     = Invalid (INV8_Collision_size n) p
                           | otherwise = Valid
        distinctKeys = A.foldMap (\(L k _) -> appearsOnce k) ary
        appearsOnce k | A.foldMap (\(L k' _) -> if k' == k then Sum @Int 1 else Sum 0) ary == 1 = Valid
                      | otherwise = Invalid (INV9_Collision_duplicate_key k h) p

    validBitmapIndexed p b ary = validArraySize <> validSubTrees p b ary
      where
        n = A.length ary
        validArraySize | n < 1 || n >= maxChildren = Invalid (INV2_bad_BitmapIndexed_size n) p
                       | popCount b == n           = Valid
                       | otherwise                 = Invalid (INV3_bitmap_array_size_mismatch b n) p

    validSubTrees p b ary
      | A.length ary == 1
      , isLeafOrCollision (A.index ary 0)
      = Invalid INV4_BitmapIndexed_invalid_single_subtree p
      | otherwise = go b
      where
        go 0  = Valid
        go b' = validInternal (addSubHash p (fromIntegral c)) (A.index ary i) <> go b''
          where
            c = countTrailingZeros b'
            m = 1 `unsafeShiftL` c
            i = sparseIndex b m
            b'' = b' .&. complement m

    validFull p ary = validArraySize <> validSubTrees p fullBitmap ary
      where
        n = A.length ary
        validArraySize | n == maxChildren = Valid
                       | otherwise        = Invalid (INV7_bad_Full_size n) p
