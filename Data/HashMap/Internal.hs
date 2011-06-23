{-# LANGUAGE DeriveDataTypeable #-}

module Data.HashMap.Internal
    ( hash
    , Leaf(..)
    , HashMap(..)
    , Hash
    , Bitmap
    , Shift
    , bitsPerSubkey
    , subkeyMask
    , index
    , bitpos
    , mask
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.Bits ((.&.))
import Data.Word (Word)

import qualified Data.HashMap.Array as A
import qualified Data.Hashable as H
import Data.HashMap.PopCount (popCount)
import Data.HashMap.UnsafeShift (unsafeShiftL, unsafeShiftR)
import Data.Typeable (Typeable)

------------------------------------------------------------------------

-- | Convenience function.  Compute a hash value for the given value.
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L {-# UNPACK #-} !Hash !k v

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L _ k v) = rnf k `seq` rnf v

-- | A mapping from keys to values.  Keys are required to be
-- 'H.Hashable'.
data HashMap k v
    = Empty
    | BitmapIndexed {-# UNPACK #-} !Bitmap {-# UNPACK #-} !(A.Array (HashMap k v))
    | Leaf {-# UNPACK #-} !(Leaf k v)
    | Full {-# UNPACK #-} !(A.Array (HashMap k v))
    | Collision {-# UNPACK #-} !Hash {-# UNPACK #-} !(A.Array (Leaf k v))
      deriving (Typeable)

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf (L _ k v))      = rnf k `seq` rnf v
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

type Hash   = Word
type Bitmap = Word
type Shift  = Int

bitsPerSubkey :: Int
bitsPerSubkey = 4

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

index :: Bitmap -> Bitmap -> Int
index b m = popCount (b .&. (m - 1))

bitpos :: Word -> Shift -> Bitmap
bitpos h s = 1 `unsafeShiftL` mask h s
{-# INLINE bitpos #-}

mask :: Word -> Shift -> Int
mask h s = fromIntegral $ unsafeShiftR h s .&. subkeyMask
{-# INLINE mask #-}
