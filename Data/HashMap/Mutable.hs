{-# LANGUAGE BangPatterns, Rank2Types #-}

module Data.HashMap.Mutable
    ( HashMap
    , empty
    , insert
    , fromList
    , freeze
    ) where

import Control.Monad.ST
import Data.Bits ((.&.), (.|.))
import Data.Hashable (Hashable)
import Prelude hiding (map)

import qualified Data.HashMap.Array as A
import Data.HashMap.Internal hiding (HashMap(..), Leaf(..))
import qualified Data.HashMap.Internal as I

data Leaf k v = L {-# UNPACK #-} !Hash !k v

-- | A mutable map.
data HashMap s k v
    = Empty
    | BitmapIndexed {-# UNPACK #-} !Bitmap
                    {-# UNPACK #-} !(A.MArray s (HashMap s k v))
    | Leaf {-# UNPACK #-} !(Leaf k v)
    | Full {-# UNPACK #-} !(A.MArray s (HashMap s k v))
    | Collision {-# UNPACK #-} !Hash
                {-# UNPACK #-} !(A.MArray s (Leaf k v))

updateOrSnoc :: Eq k => Hash -> k -> v -> A.MArray s (Leaf k v)
             -> ST s (A.MArray s (Leaf k v))
updateOrSnoc h0 k0 v0 mary0 = go h0 k0 v0 mary0 0 (A.lengthM mary0)
  where
    go !h !k v !mary !i !n
        | i >= n = do
            -- Not found, append to the end.
            mary2 <- A.new (n + 1) undefinedElem
            unsafeCopy mary 0 mary2 0 n
            A.unsafeWrite mary2 n (L h k v)
            return mary2
        | otherwise = do
            (L hx kx _) <- A.unsafeRead mary i
            if h == hx && k == kx
                then A.unsafeWrite mary i (L h k v) >> return mary
                else go h k v mary (i+1) n
{-# INLINABLE updateOrSnoc #-}

undefinedElem :: a
undefinedElem = error "Undefined element!"

------------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Construct an empty 'HashMap'.
empty :: HashMap s k v
empty = Empty

-- | Create a 'Collision' value with two 'Leaf' values.
collision :: Hash -> Leaf k v -> Leaf k v -> ST s (HashMap s k v)
collision h e1 e2 = do
    mary <- A.new 2 e1
    A.unsafeWrite mary 1 e2
    return $! Collision h mary
{-# INLINE collision #-}

-- | /O(min(n,W))/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap s k v -> ST s (HashMap s k v)
insert k0 v0 = go h0 k0 v0 0
  where
    h0 = hash k0
    go !h !k x !_ Empty = return $! Leaf (L h k x)
    go h k x s t@(Leaf (L hy ky y))
        | hy == h = if ky == k
                    then return $! Leaf (L h k x)
                    else collision h (L hy ky y) (L h k x)
        | otherwise = do
            mary <- A.new 1 t
            go h k x s $ BitmapIndexed (bitpos hy s) mary
    go h k x s t@(BitmapIndexed b mary) =
        let m = bitpos h s
            i = index b m
        in if b .&. m == 0
               then let l     = Leaf (L h k x)
                        b'    = b .|. m
                    in do mary' <- unsafeInsert mary i $! l
                          if b' == 0xFFFFFFFF
                              then return $! Full mary'
                              else return $! BitmapIndexed b' mary'
               else do st <- A.unsafeRead mary i
                       st' <- go h k x (s+bitsPerSubkey) st
                       A.unsafeWrite mary i st'
                       return t
    go h k x s (Full mary) =
        let i = mask h s
        in do st <- A.unsafeRead mary i
              st' <- go h k x (s+bitsPerSubkey) st
              A.unsafeWrite mary i st'
              return $! Full mary
    go h k x s t@(Collision hy v)
        | h == hy = do v' <- updateOrSnoc h k x v
                       return $! Collision h v'
        | otherwise = do v' <- A.new 1 t
                         go h k x s $ BitmapIndexed (bitpos hy s) v'
{-# INLINABLE insert #-}
                         
fromList :: (Eq k, Hashable k) => [(k, v)] -> ST s (HashMap s k v)
fromList = foldlM' (flip $ uncurry insert) empty
{-# INLINE fromList #-}

freeze :: HashMap s k v -> ST s (I.HashMap k v)
freeze = go
  where
    go Empty = return I.Empty
    go (BitmapIndexed b mary) = do mary' <- map go mary
                                   ary <- A.unsafeFreeze mary'
                                   return $! I.BitmapIndexed b ary
    go (Leaf (L h k v)) = return $! I.Leaf (I.L h k v)
    go (Full mary) = do mary' <- map go mary
                        ary <- A.unsafeFreeze mary'
                        return $! I.Full ary
    go (Collision h mary) = do
        mary' <- map (\ (L h' k v) -> return $! (I.L h' k v)) mary
        ary <- A.unsafeFreeze mary'
        return $! I.Collision h ary

------------------------------------------------------------------------

foldlM' :: (a -> b -> ST s a) -> a -> [b] -> ST s a
foldlM' f = go
  where go !z []    = return z
        go z (x:xs) = do z' <- f z x
                         go z' xs
{-# INLINE foldlM' #-}

-- | Unsafely copy the elements of an array. Array bounds are not checked.
unsafeCopy :: A.MArray s e -> Int -> A.MArray s e -> Int -> Int -> ST s ()
unsafeCopy !src !sidx !dest !didx count =
        copy_loop sidx didx 0
  where
    copy_loop !i !j !c
        | c >= count = return ()
        | otherwise = do b <- A.unsafeRead src i
                         A.unsafeWrite dest j b
                         copy_loop (i+1) (j+1) (c+1)

unsafeInsert :: A.MArray s e -> Int -> e -> ST s (A.MArray s e)
unsafeInsert src idx b = do
    dest <- A.new (count+1) undefinedElem
    unsafeCopy src 0 dest 0 idx
    A.unsafeWrite dest idx b
    unsafeCopy src idx dest (idx+1) (count-idx)
    return dest
  where !count = A.lengthM src
{-# INLINE unsafeInsert #-}

map :: (a -> ST s b) -> A.MArray s a -> ST s (A.MArray s b)
map f mary = do
        mary2 <- A.new count undefinedElem
        let go !i
                | i >= count = return ()
                | otherwise = do
                    a <- A.unsafeRead mary i
                    b <- f a
                    A.unsafeWrite mary2 i b
                    go (i+1)
        go 0
        return mary2
  where
    !count = A.lengthM mary
{-# INLINE map #-}
