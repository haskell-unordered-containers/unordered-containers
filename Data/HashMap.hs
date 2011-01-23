{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Data.HashMap
    ( HashMap
    , empty
    , null
    , insert
    , lookup
    , fromList
    , toList
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.Bits ((.&.), (.|.), bitSize)
import qualified Data.Hashable as H
import Data.Hashable (Hashable)
import qualified Data.List as L
import GHC.Exts (Word(W#), Int(I#), uncheckedShiftL#, uncheckedShiftRL#)
import Prelude hiding (lookup, null)

import qualified Data.HashMap.Array as A
import Data.HashMap.PopCount (popCount)

------------------------------------------------------------------------

-- | Convenience function.  Compute a hash value for the given value.
hash :: Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L {-# UNPACK #-} !Hash !k v

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L _ k v) = rnf k `seq` rnf v

data HashMap k v
    = Empty
    | BitmapIndexed {-# UNPACK #-} !Bitmap {-# UNPACK #-} !(A.Array (HashMap k v))
    | Leaf {-# UNPACK #-} !(Leaf k v)
    | Full {-# UNPACK #-} !(A.Array (HashMap k v))
    | Collision {-# UNPACK #-} !Hash {-# UNPACK #-} !(A.Array (Leaf k v))

type Hash   = Word
type Bitmap = Word
type Shift  = Int
type Subkey = Int -- we need to use this to do shifts, so an Int it is

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf (L _ k v))      = rnf k `seq` rnf v
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

-- These architecture dependent constants

bitsPerSubkey :: Int
bitsPerSubkey
    | wordSizeInBits == 32 = 5
    | wordSizeInBits == 64 = 6
    | otherwise =
        error "Data.HashMap.bitsPerSubkey: Unsupported architecture"
  where wordSizeInBits = bitSize (undefined :: Word)

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

maskIndex :: Bitmap -> Bitmap -> Int
maskIndex b m = popCount (b .&. (m - 1))

mask :: Word -> Shift -> Bitmap
mask k s = 1 `unsafeShiftL` subkey k s
{-# INLINE mask #-}

subkey :: Word -> Shift -> Int
subkey k s = fromIntegral $ unsafeShiftR k s .&. subkeyMask
{-# INLINE subkey #-}

-- | /O(n)/ Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
lookupInArray :: Eq k => Hash -> k -> A.Array (Leaf k v) -> Maybe v
lookupInArray h0 k0 ary0 = go h0 k0 ary0 0 (A.length ary0)
  where
    go !h !k !ary !i !n
        | i >= n = Nothing
        | otherwise = case A.unsafeIndex ary i of
            (L hx kx v)
                | h == hx && k == kx -> Just v
                | otherwise -> go h k ary (i+1) n
{-# INLINABLE lookupInArray #-}

updateOrSnoc :: Eq k => Hash -> k -> v -> A.Array (Leaf k v)
             -> A.Array (Leaf k v)
updateOrSnoc h0 k0 v0 ary0 = go h0 k0 v0 ary0 0 (A.length ary0)
  where
    go !h !k v !ary !i !n
        | i >= n = A.run $ do
            -- Not found, append to the end.
            mary <- A.new (n + 1) undefinedElem
            A.unsafeCopy ary 0 mary 0 n
            A.unsafeWrite mary n (L h k v)
            return mary
        | otherwise = case A.unsafeIndex ary i of
            (L hx kx _)
                | h == hx && k == kx -> A.unsafeUpdate ary i (L h k v)
                | otherwise -> go h k v ary (i+1) n
{-# INLINABLE updateOrSnoc #-}

undefinedElem :: a
undefinedElem = error "Undefined element!"

------------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Construct an empty 'HashMap'.
empty :: HashMap k v
empty = Empty

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: HashMap k v -> Bool
null Empty = True
null _   = False

-- | /O(min(n,W))/ Return the value to which the specified key is
-- mapped, or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Nothing
    go h k _ (Leaf (L hx kx x))
        | h == hx && k == kx = Just x
        | otherwise = Nothing
    go h k s (BitmapIndexed b v) =
        let m = mask h s
        in if b .&. m == 0
           then Nothing
           else go h k (s+bitsPerSubkey) (A.unsafeIndex v (maskIndex b m))
    go h k s (Full v) = go h k (s+bitsPerSubkey) (A.unsafeIndex v (subkey h s))
    go h k _ (Collision hx v)
        | h == hx   = lookupInArray h k v
        | otherwise = Nothing
{-# INLINABLE lookup #-}

-- | Create a 'Collision' value with two 'Leaf' values.
collision :: Hash -> Leaf k v -> Leaf k v -> HashMap k v
collision h e1 e2 =
    let v = A.run $ do mary <- A.new 2 e1
                       A.unsafeWrite mary 1 e2
                       return mary
    in Collision h v
{-# INLINE collision #-}

-- | /O(min(n,W))/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k0 v0 = go h0 k0 v0 0
  where
    h0 = hash k0
    go !h !k x !_ Empty = Leaf (L h k x)
    go h k x s t@(Leaf (L hy ky y))
        | hy == h = if ky == k
                    then Leaf (L h k x)
                    else collision h (L hy ky y) (L h k x)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
    go h k x s (BitmapIndexed b ary) =
        let m = mask h s
            i = maskIndex b m
        in if b .&. m == 0
               then let l    = Leaf (L h k x)
                        ary' = A.unsafeInsert ary i $! l
                        b'   = b .|. m
                    in if b' == 0xFFFFFFFF
                       then Full ary'
                       else BitmapIndexed b' ary'
               else let  st   = A.unsafeIndex ary i
                         st'  = go h k x (s+bitsPerSubkey) st
                         ary' = A.unsafeUpdate ary i $! st'
                    in BitmapIndexed b ary'
    go h k x s (Full ary) =
        let i    = subkey h s
            st   = A.unsafeIndex ary i
            st'  = go h k x (s+bitsPerSubkey) st
            ary' = A.unsafeUpdate ary i $! st'
        in Full ary'
    go h k x s t@(Collision hy v)
        | h == hy = Collision h (updateOrSnoc h k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
{-# INLINABLE insert #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the identity of
-- the operator).
fold :: (k -> v -> a -> a) -> a -> HashMap k v -> a
fold f = go
  where
    go z Empty = z
    go z (Leaf (L _ k v)) = f k v z
    go z (BitmapIndexed _ ary) = A.foldr (flip go) z ary
    go z (Full ary) = A.foldr (flip go) z ary
    go z (Collision _ ary) = A.foldr (\ (L _ k v) z' -> f k v z') z ary
{-# INLINE fold #-}

fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = L.foldl' (flip $ uncurry insert) empty
{-# INLINABLE fromList #-}

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily.
toList :: HashMap k v -> [(k, v)]
toList = fold (\ k v xs -> (k, v) : xs) []
{-# INLINABLE toList #-}

------------------------------------------------------------------------
-- Utility functions

unsafeShiftL :: Word -> Int -> Word
unsafeShiftL (W# x#) (I# i#) = W# (x# `uncheckedShiftL#` i#)
{-# INLINE unsafeShiftL #-}

unsafeShiftR :: Word -> Int -> Word
unsafeShiftR (W# x#) (I# i#) = W# (x# `uncheckedShiftRL#` i#)
{-# INLINE unsafeShiftR #-}

