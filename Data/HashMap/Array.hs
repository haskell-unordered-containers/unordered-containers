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
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

data Array a = Array {
    unArray :: !(Array# a), length :: {-# UNPACK #-} !Int }
data MArray s a = MArray {
    unMArray :: !(MutableArray# s a), lengthM :: {-# UNPACK #-} !Int }

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
    (# s', ary #) -> (# s', MArray ary n #)
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

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze (MArray mary n)
    = ST $ \s -> case unsafeFreezeArray# mary s of
                   (# s', ary #) -> (# s', Array ary n #)
{-# INLINE unsafeFreeze #-}

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

-- | Unsafely copy the elements of an array. Array bounds are not checked.
unsafeCopy :: Array e -> Int -> MArray s e -> Int -> Int -> ST s ()
unsafeCopy !src !sidx !dest !didx count =
#if defined(ASSERTS)
    assert (sidx + count <= length src) .
    assert (didx + count <= lengthM dest) $
#endif
    copy_loop sidx didx 0
    where
      copy_loop !i !j !c
          | c >= count = return ()
          | otherwise = do unsafeWrite dest j $! unsafeIndex src i
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
