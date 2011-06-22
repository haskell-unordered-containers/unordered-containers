{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples #-}

module Data.HashMap.Array
    ( Array
    , MArray
    , new
    , singleton
    , length
    , lengthM
    , unsafeRead
    , unsafeWrite
    , unsafeIndex
    , unsafeFreeze
    , run
    , unsafeCopy
    , unsafeUpdate
    , unsafeInsert
    , foldr
    , unsafeUpdate32
    ) where

import Control.DeepSeq
import Control.Monad.ST
import GHC.Exts
import GHC.ST (ST(..))
import Prelude hiding (foldr, length)

------------------------------------------------------------------------

#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
# define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("Data.HashMap.Array." ++ (_func_) ++ ": bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else
# define CHECK_LENGTH(_func_,_expected_,_actual_) \
if (_actual_) /= (_expected_) then error ("Data.HashMap.Array." ++ (_func_) ++ ": expected length " ++ show (_expected_) ++ ", actual length " ++ show (_actual_)) else
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
# define CHECK_LENGTH(_func_,_len_,_actual_)
#endif

data Array a = Array {
      unArray :: !(Array# a)
#if __GLASGOW_HASKELL__ < 701
    , length :: {-# UNPACK #-} !Int
#endif
    }

#if __GLASGOW_HASKELL__ >= 701
length :: Array a -> Int
length ary = I# (sizeofArray# (unArray ary))
{-# INLINE length #-}
#endif

-- | Smart constructor
array :: Array# a -> Int -> Array a
#if __GLASGOW_HASKELL__ >= 701
array ary _n = Array ary
#else
array = Array
#endif
{-# INLINE array #-}

data MArray s a = MArray {
      unMArray :: !(MutableArray# s a)
#if __GLASGOW_HASKELL__ < 701
    , lengthM :: {-# UNPACK #-} !Int
#endif
    }

#if __GLASGOW_HASKELL__ >= 701
lengthM :: MArray s a -> Int
lengthM mary = I# (sizeofMutableArray# (unMArray mary))
{-# INLINE lengthM #-}
#endif

-- | Smart constructor
marray :: MutableArray# s a -> Int -> MArray s a
#if __GLASGOW_HASKELL__ >= 701
marray mary _n = MArray mary
#else
marray = MArray
#endif
{-# INLINE marray #-}

------------------------------------------------------------------------

instance NFData a => NFData (Array a) where
    rnf = rnfArray

rnfArray :: NFData a => Array a -> ()
rnfArray ary0 = go ary0 n0 0
  where
    n0 = length ary0
    go !ary !n !i
        | i >= n = ()
        | otherwise = rnf (unsafeIndex ary i) `seq` go ary n (i+1)
{-# INLINE rnfArray #-}

-- | Create a new mutable array of specified size, in the specified
-- state thread, with each element containing the specified initial
-- value.
new :: Int -> a -> ST s (MArray s a)
new n@(I# n#) b = ST $ \s -> case newArray# n# b s of
    (# s', ary #) -> (# s', marray ary n #)
{-# INLINE new #-}

singleton :: a -> Array a
singleton x = run (new 1 x)
{-# INLINE singleton #-}

unsafeRead :: MArray s a -> Int -> ST s a
unsafeRead ary _i@(I# i#) = ST $ \ s ->
    CHECK_BOUNDS("unsafeRead", lengthM ary, _i)
        readArray# (unMArray ary) i# s
{-# INLINE unsafeRead #-}

unsafeWrite :: MArray s a -> Int -> a -> ST s ()
unsafeWrite ary _i@(I# i#) b = ST $ \ s ->
    CHECK_BOUNDS("unsafeWrite", lengthM ary, _i)
        case writeArray# (unMArray ary) i# b s of
            s' -> (# s' , () #)
{-# INLINE unsafeWrite #-}

unsafeIndex :: Array a -> Int -> a
unsafeIndex ary _i@(I# i#) =
    CHECK_BOUNDS("unsafeIndex", length ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> b
{-# INLINE unsafeIndex #-}

unsafeIndexM :: Array a -> Int -> ST s a
unsafeIndexM ary _i@(I# i#) =
    CHECK_BOUNDS("unsafeIndexM", length ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> return b
{-# INLINE unsafeIndexM #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze mary
    = ST $ \s -> case unsafeFreezeArray# (unMArray mary) s of
                   (# s', ary #) -> (# s', array ary (lengthM mary) #)
{-# INLINE unsafeFreeze #-}

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

-- | Unsafely copy the elements of an array. Array bounds are not checked.
unsafeCopy :: Array e -> Int -> MArray s e -> Int -> Int -> ST s ()
unsafeCopy !src !sidx !dest !didx count =
    CHECK_BOUNDS("unsafeCopy", length src, sidx + count)
    CHECK_BOUNDS("unsafeCopy", lengthM dest, didx + count)
        copy_loop sidx didx 0
  where
    copy_loop !i !j !c
        | c >= count = return ()
        | otherwise = do b <- unsafeIndexM src i
                         unsafeWrite dest j b
                         copy_loop (i+1) (j+1) (c+1)

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
unsafeInsert :: Array e -> Int -> e -> Array e
unsafeInsert ary idx b =
    CHECK_BOUNDS("unsafeInsert", count + 1, idx)
        run $ do
            mary <- new (count+1) undefinedElem
            unsafeCopy ary 0 mary 0 idx
            unsafeWrite mary idx b
            unsafeCopy ary idx mary (idx+1) (count-idx)
            return mary
  where !count = length ary
{-# INLINE unsafeInsert #-}

-- | /O(n)/ Update the element at the given position in this array.
unsafeUpdate :: Array e -> Int -> e -> Array e
unsafeUpdate ary idx b =
    CHECK_BOUNDS("unsafeUpdate", count, idx)
        run $ do
            mary <- new count undefinedElem
            unsafeCopy ary 0 mary 0 count
            unsafeWrite mary idx b
            return mary
  where !count = length ary
{-# INLINE unsafeUpdate #-}

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i z
        | i >= n    = z
        | otherwise = f (unsafeIndex ary i) (go ary n (i+1) z)
{-# INLINE foldr #-}

undefinedElem :: a
undefinedElem = error "Undefined element!"

------------------------------------------------------------------------
-- Manually unrolled loops

-- | /O(n)/ Update the element at the given position in this array.
unsafeUpdate32 :: Array e -> Int -> e -> Array e
unsafeUpdate32 ary idx b =
    CHECK_BOUNDS("unsafeUpdate32", length ary, idx)
        run $ do
            mary <- unsafeClone32 ary
            unsafeWrite mary idx b
            return mary
{-# INLINE unsafeUpdate32 #-}

-- | Unsafely clone an array of 32 elements.  The length of the input
-- array is not checked.
unsafeClone32 :: Array e -> ST s (MArray s e)
unsafeClone32 ary =
    CHECK_LENGTH("unsafeClone32", 32, length ary)
#if __GLASGOW_HASKELL__ >= 701
        ST $ \ s -> case thawArray# (unArray ary) 0# 32# s of
            (# s2, ary# #) -> (# s2, marray ary# 32 #)
#else
        do mary <- new 32 undefinedElem
           unsafeIndexM ary 0 >>= unsafeWrite mary 0
           unsafeIndexM ary 1 >>= unsafeWrite mary 1
           unsafeIndexM ary 2 >>= unsafeWrite mary 2
           unsafeIndexM ary 3 >>= unsafeWrite mary 3
           unsafeIndexM ary 4 >>= unsafeWrite mary 4
           unsafeIndexM ary 5 >>= unsafeWrite mary 5
           unsafeIndexM ary 6 >>= unsafeWrite mary 6
           unsafeIndexM ary 7 >>= unsafeWrite mary 7
           unsafeIndexM ary 8 >>= unsafeWrite mary 8
           unsafeIndexM ary 9 >>= unsafeWrite mary 9
           unsafeIndexM ary 10 >>= unsafeWrite mary 10
           unsafeIndexM ary 11 >>= unsafeWrite mary 11
           unsafeIndexM ary 12 >>= unsafeWrite mary 12
           unsafeIndexM ary 13 >>= unsafeWrite mary 13
           unsafeIndexM ary 14 >>= unsafeWrite mary 14
           unsafeIndexM ary 15 >>= unsafeWrite mary 15
           unsafeIndexM ary 16 >>= unsafeWrite mary 16
           unsafeIndexM ary 17 >>= unsafeWrite mary 17
           unsafeIndexM ary 18 >>= unsafeWrite mary 18
           unsafeIndexM ary 19 >>= unsafeWrite mary 19
           unsafeIndexM ary 20 >>= unsafeWrite mary 20
           unsafeIndexM ary 21 >>= unsafeWrite mary 21
           unsafeIndexM ary 22 >>= unsafeWrite mary 22
           unsafeIndexM ary 23 >>= unsafeWrite mary 23
           unsafeIndexM ary 24 >>= unsafeWrite mary 24
           unsafeIndexM ary 25 >>= unsafeWrite mary 25
           unsafeIndexM ary 26 >>= unsafeWrite mary 26
           unsafeIndexM ary 27 >>= unsafeWrite mary 27
           unsafeIndexM ary 28 >>= unsafeWrite mary 28
           unsafeIndexM ary 29 >>= unsafeWrite mary 29
           unsafeIndexM ary 30 >>= unsafeWrite mary 30
           unsafeIndexM ary 31 >>= unsafeWrite mary 31
           return mary
#endif
