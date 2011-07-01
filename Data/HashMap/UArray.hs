-- | Manually unpacked version of `Array (Either t (k, v))`.
module Data.HashMap.UArray where

import qualified Data.HashMap.Array as A
import Data.Bits

type Bitmap = Word

data UArray a b c
    = UA {-# UNPACK #-} !BitMap       -- element in the first array?
         {-# UNPACK #-} !(A.Array a)
         {-# UNPACK #-} !(A.Array b)
         {-# UNPACK #-} !(A.Array c)

index :: UArray a b c -> Int -> Either a (b, c)
index (UA bm as bs cs) i
    | m .&. bm == 0 = Right (A.index ks m, A.index vs m)
    | otherwise = Left $ A.index ts m
  where m = bit i
{-# INLINE index #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insert :: UArray a b c -> Int -> (Either a (b, c)) -> UArray a b c
insert ary idx b =
    CHECK_BOUNDS("insert", count + 1, idx)
        run $ do
            mary <- new (count+1) undefinedElem
            copy ary 0 mary 0 idx
            write mary idx b
            copy ary idx mary (idx+1) (count-idx)
            return mary
  where !count = length ary
{-# INLINE insert #-}

-- | Number of bits set among the least significant i-1 bits.
popcnt :: Bitmap -> Int -> Int
popcnt b i = popCount (b .&. (bit i - 1))

-- | A 'Bitmap' with bit @i@ set.
bit :: Int -> Bitmap
bit i = 1 `unsafeShiftL` i
{-# INLINE bitpos #-}
