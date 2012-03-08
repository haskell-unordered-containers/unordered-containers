{-# LANGUAGE BangPatterns, CPP #-}

-- | Operations in ST.
module Data.HashMap.Mutable
    ( insertWith
    , unsafeFreeze
    ) where

import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.))
import Data.Hashable (Hashable)

import qualified Data.HashMap.Array as A
import qualified Data.HashMap.Base as B
import Data.HashMap.Bits

-- Invariant: The length of the 1st argument to 'Full' is
-- 2^bitsPerSubkey

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap s k v
    = Empty
    | BitmapIndexed !Bitmap !(A.MArray s (HashMap s k v))
    | Leaf !Hash !(Leaf k v)
    | Full !(A.MArray s (HashMap s k v))
    | Collision !Hash !(A.MArray s (Leaf k v))

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> A.MArray s (HashMap s k v) -> HashMap s k v
bitmapIndexedOrFull b mary
    | b == fullNodeMask = Full mary
    | otherwise         = BitmapIndexed b mary
{-# INLINE bitmapIndexedOrFull #-}

-- | Create a 'Collision' value with two 'Leaf' values.
collision :: Hash -> Leaf k v -> Leaf k v -> ST s (HashMap s k v)
collision h e1 e2 = do
    mary <- A.new 2 e1
    A.write mary 1 e2
    return $! Collision h mary
{-# INLINE collision #-}

-- | Create a map from two key-value pairs which hashes don't collide.
two :: Shift -> Hash -> k -> v -> Hash -> k -> v -> ST s (HashMap s k v)
two = go
  where
    go s h1 k1 v1 h2 k2 v2
        | bp1 == bp2 = do
            st <- go (s+bitsPerSubkey) h1 k1 v1 h2 k2 v2 
            mary <- A.new 1 st
            return $! BitmapIndexed bp1 mary
        | otherwise  = do
            mary <- A.new 2 $ Leaf h1 (L k1 v1)
            A.write mary idx2 $ Leaf h2 (L k2 v2)
            return $! BitmapIndexed (bp1 .|. bp2) mary
      where
        bp1  = mask h1 s
        bp2  = mask h2 s
        idx2 | index h1 s < index h2 s = 1
             | otherwise               = 0
{-# INLINE two #-}

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap s k v
           -> ST s (HashMap s k v)
insertWith f k0 v0 m0 = go h0 k0 v0 0 m0
  where
    h0 = hash k0
    go !h !k x !_ Empty = return $! Leaf h (L k x)
    go h k x s (Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then return $! Leaf h (L k (f x y))
                    else collision h l (L k x)
        | otherwise = two s h k x hy ky y
    go h k x s (BitmapIndexed b mary)
        | b .&. m == 0 = do
            mary' <- A.insertM mary i $! Leaf h (L k x)
            return $! bitmapIndexedOrFull (b .|. m) mary'
        | otherwise = do
            st <- A.read mary i
            st' <- go h k x (s+bitsPerSubkey) st
            A.write mary i st'
            return $! BitmapIndexed b mary
      where m = mask h s
            i = sparseIndex b m
    go h k x s (Full mary) = do
        st <- A.read mary i
        st' <- go h k x (s+bitsPerSubkey) st
        A.write mary i st'
        return $! Full mary
      where i = index h s
    go h k x s t@(Collision hy mary)
        | h == hy   = do
            mary' <- updateOrSnocWith f k x mary
            return $! Collision h mary'
        | otherwise = do
            st <- A.new 1 t
            go h k x s $ BitmapIndexed (mask hy s) st
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

unsafeFreeze :: HashMap s k v -> ST s (B.HashMap k v)
unsafeFreeze = go
  where
    go Empty                   = return B.Empty
    go (Leaf h l)              = return $! B.Leaf h l
    go (BitmapIndexed bm mary) = do mary' <- A.mapM' go mary
                                    ary <- A.unsafeFreeze mary'
                                    return $! B.BitmapIndexed bm ary
    go (Full mary)             = do mary' <- A.mapM' go mary
                                    ary <- A.unsafeFreeze mary'
                                    return $! B.Full ary
    go (Collision h mary)      = do ary <- A.unsafeFreeze mary
                                    return $! B.Collision h ary

------------------------------------------------------------------------
-- Array operations

updateOrSnocWith :: Eq k => (v -> v -> v) -> k -> v -> A.MArray s (Leaf k v)
                 -> ST s (A.MArray s (Leaf k v))
updateOrSnocWith f k0 v0 mary0 = go k0 v0 mary0 0 (A.lengthM mary0)
  where
    go !k v !mary !i !n
        | i >= n = do
            -- Not found, append to the end.
            mary' <- A.new_ (n + 1)
            A.copyM mary 0 mary' 0 n
            A.write mary' n (L k v)
            return mary'
        | otherwise = do
            leaf <- A.read mary i
            case leaf of
                (L kx y) | k == kx   -> do A.write mary i (L k (f v y))
                                           return mary
                         | otherwise -> go k v mary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateOrSnocWith #-}
#endif
