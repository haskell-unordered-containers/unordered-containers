{-# LANGUAGE BangPatterns, CPP #-}

module Data.HashMap
    ( HashMap
    , empty
    , singleton
    , null
    , insert
    , lookup
    , fromList
    , toList
    ) where

import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.))
import Data.Hashable (Hashable)
import Prelude hiding (lookup, null)

import qualified Data.HashMap.Array as A
import Data.HashMap.Internal
import qualified Data.HashMap.Mutable as M

------------------------------------------------------------------------

-- These two instances are here to avoid having
-- 'Data.HashMap.Internal' having to depend on 'Data.HashMap.Mutable',
-- which would create an import cycle.

instance (Show k, Show v) => Show (HashMap k v) where
    show m = "fromList " ++ show (toList m)

-- NOTE: This is just a placeholder.
instance (Eq k, Eq v) => Eq (HashMap k v) where
    a == b = toList a == toList b

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

-- | /O(1)/ Construct an empty map.
empty :: HashMap k v
empty = Empty

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: HashMap k v -> Bool
null Empty = True
null _   = False

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Nothing
    go h k _ (Leaf (L hx kx x))
        | h == hx && k == kx = Just x
        | otherwise = Nothing
    go h k s (BitmapIndexed b v) =
        let m = bitpos h s
        in if b .&. m == 0
           then Nothing
           else go h k (s+bitsPerSubkey) (A.unsafeIndex v (index b m))
    go h k s (Full v) = go h k (s+bitsPerSubkey) (A.unsafeIndex v (mask h s))
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

-- | /O(1)/ Construct a map with a single element.
singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k v = Leaf (L (hash k) k v)

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k0 v0 = go h0 k0 v0 0
  where
    h0 = hash k0
    go !h !k x !_ Empty = Leaf (L h k x)
    go h k x s t@(Leaf l@(L hy ky _))
        | hy == h = if ky == k
                    then Leaf (L h k x)
                    else collision h l (L h k x)
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
    go h k x s (BitmapIndexed b ary) =
        let m = bitpos h s
            i = index b m
        in if b .&. m == 0
               then let l    = Leaf (L h k x)
                        ary' = A.unsafeInsert ary i $! l
                        b'   = b .|. m
                    in if b' == 0xFFFF
                       then Full ary'
                       else BitmapIndexed b' ary'
               else let  st   = A.unsafeIndex ary i
                         st'  = go h k x (s+bitsPerSubkey) st
                         ary' = A.unsafeUpdate ary i $! st'
                    in BitmapIndexed b ary'
    go h k x s (Full ary) =
        let i    = mask h s
            st   = A.unsafeIndex ary i
            st'  = go h k x (s+bitsPerSubkey) st
            ary' = unsafeUpdate16 ary i $! st'
        in Full ary'
    go h k x s t@(Collision hy v)
        | h == hy = Collision h (updateOrSnoc h k x v)
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
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

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList xs = runST (M.freeze =<< M.fromList xs)
{-# INLINABLE fromList #-}

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily.
toList :: HashMap k v -> [(k, v)]
toList = fold (\ k v xs -> (k, v) : xs) []
{-# INLINABLE toList #-}

------------------------------------------------------------------------
-- Manually unrolled loops

-- | /O(n)/ Update the element at the given position in this array.
unsafeUpdate16 :: A.Array e -> Int -> e -> A.Array e
unsafeUpdate16 ary idx b =
    A.run $ do
        mary <- unsafeClone16 ary
        A.unsafeWrite mary idx b
        return mary
{-# INLINE unsafeUpdate16 #-}

-- | Unsafely clone an array of 16 elements.  The length of the input
-- array is not checked.
unsafeClone16 :: A.Array e -> ST s (A.MArray s e)
unsafeClone16 ary =
#if __GLASGOW_HASKELL__ >= 701
    A.thaw ary 0 16
#else
    do mary <- new 16 undefinedElem
       A.unsafeIndexM ary 0 >>= A.unsafeWrite mary 0
       A.unsafeIndexM ary 1 >>= A.unsafeWrite mary 1
       A.unsafeIndexM ary 2 >>= A.unsafeWrite mary 2
       A.unsafeIndexM ary 3 >>= A.unsafeWrite mary 3
       A.unsafeIndexM ary 4 >>= A.unsafeWrite mary 4
       A.unsafeIndexM ary 5 >>= A.unsafeWrite mary 5
       A.unsafeIndexM ary 6 >>= A.unsafeWrite mary 6
       A.unsafeIndexM ary 7 >>= A.unsafeWrite mary 7
       A.unsafeIndexM ary 8 >>= A.unsafeWrite mary 8
       A.unsafeIndexM ary 9 >>= A.unsafeWrite mary 9
       A.unsafeIndexM ary 10 >>= A.unsafeWrite mary 10
       A.unsafeIndexM ary 11 >>= A.unsafeWrite mary 11
       A.unsafeIndexM ary 12 >>= A.unsafeWrite mary 12
       A.unsafeIndexM ary 13 >>= A.unsafeWrite mary 13
       A.unsafeIndexM ary 14 >>= A.unsafeWrite mary 14
       A.unsafeIndexM ary 15 >>= A.unsafeWrite mary 15
       return mary
#endif
