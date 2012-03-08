-- | Types and functions for working with HAMT bitmaps.
module Data.HashMap.Bits
    ( Bitmap
    , Hash
    , Shift
    , Leaf(..)
    , bitsPerSubkey
    , fullNodeMask
    , hash
    , index
    , mask
    , maxChildren
    , sparseIndex
    , subkeyMask  
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.Bits ((.&.), complement)
import qualified Data.Hashable as H
import Data.Word (Word)

import Data.HashMap.PopCount (popCount)
import Data.HashMap.UnsafeShift (unsafeShiftL, unsafeShiftR)

-- TODO: Rename to Pair or some such.
-- TODO: This doesn't quite belong here.
data Leaf k v = L !k v

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L k v) = rnf k `seq` rnf v

type Hash   = Word
type Bitmap = Word
type Shift  = Int

-- | Convenience function.  Compute a hash value for the given value.
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

bitsPerSubkey :: Int
bitsPerSubkey = 4

-- | The maximum number children of a node.
maxChildren :: Int
maxChildren = fromIntegral $ 1 `unsafeShiftL` bitsPerSubkey

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))

mask :: Word -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}

-- | Mask out the 'bitsPerSubkey' bits used for indexing at this level
-- of the tree.
index :: Hash -> Shift -> Int
index w s = fromIntegral $ (unsafeShiftR w s) .&. subkeyMask
{-# INLINE index #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL`
                           fromIntegral (1 `unsafeShiftL` bitsPerSubkey))
{-# INLINE fullNodeMask #-}
