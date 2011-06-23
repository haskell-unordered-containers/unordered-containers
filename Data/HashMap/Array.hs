{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples #-}

-- | Zero based arrays.
--
-- Note that no bounds checking are performed.
module Data.HashMap.Array
    ( Array
    , MArray
    , new
    , singleton
    , length
    , lengthM
    , read
    , write
    , index
    , indexM
    , unsafeFreeze
    , run
    , copy
    , update
    , insert
    , foldl'
    , foldr
    , thaw
    , delete
    , map
    ) where

import Control.DeepSeq
import Control.Monad.ST
import GHC.Exts
import GHC.ST (ST(..))
import Prelude hiding (foldr, length, map, read)

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
        | otherwise = rnf (index ary i) `seq` go ary n (i+1)
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

read :: MArray s a -> Int -> ST s a
read ary _i@(I# i#) = ST $ \ s ->
    CHECK_BOUNDS("read", lengthM ary, _i)
        readArray# (unMArray ary) i# s
{-# INLINE read #-}

write :: MArray s a -> Int -> a -> ST s ()
write ary _i@(I# i#) b = ST $ \ s ->
    CHECK_BOUNDS("write", lengthM ary, _i)
        case writeArray# (unMArray ary) i# b s of
            s' -> (# s' , () #)
{-# INLINE write #-}

index :: Array a -> Int -> a
index ary _i@(I# i#) =
    CHECK_BOUNDS("index", length ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> b
{-# INLINE index #-}

indexM :: Array a -> Int -> ST s a
indexM ary _i@(I# i#) =
    CHECK_BOUNDS("indexM", length ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> return b
{-# INLINE indexM #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze mary
    = ST $ \s -> case unsafeFreezeArray# (unMArray mary) s of
                   (# s', ary #) -> (# s', array ary (lengthM mary) #)
{-# INLINE unsafeFreeze #-}

run :: (forall s . ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copy :: Array e -> Int -> MArray s e -> Int -> Int -> ST s ()
#if __GLASGOW_HASKELL__ >= 701
copy !src !_sidx@(I# sidx#) !dst !_didx@(I# didx#) _n@(I# n#) =
    CHECK_BOUNDS("copy", length src, _sidx + _n)
    CHECK_BOUNDS("copy", lengthM dst, _didx + _n)
        ST $ \ s# ->
        case copyArray# (unArray src) sidx# (unMArray dst) didx# n# s# of
            s2 -> (# s2, () #)
#else
copy !src !sidx !dst !didx n =
    CHECK_BOUNDS("copy", length src, sidx + n)
    CHECK_BOUNDS("copy", lengthM dst, didx + n)
        copy_loop sidx didx 0
  where
    copy_loop !i !j !c
        | c >= n = return ()
        | otherwise = do b <- indexM src i
                         write dst j b
                         copy_loop (i+1) (j+1) (c+1)
#endif

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insert :: Array e -> Int -> e -> Array e
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

-- | /O(n)/ Update the element at the given position in this array.
update :: Array e -> Int -> e -> Array e
update ary idx b =
    CHECK_BOUNDS("update", count, idx)
        run $ do
            mary <- new count undefinedElem
            copy ary 0 mary 0 count
            write mary idx b
            return mary
  where !count = length ary
{-# INLINE update #-}
        
foldl' :: (b -> a -> b) -> b -> Array a -> b
foldl' f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i !z
        | i >= n    = z
        | otherwise = go ary n (i+1) (f z (index ary i))
{-# INLINE foldl' #-}

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f = \ z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i z
        | i >= n    = z
        | otherwise = f (index ary i) (go ary n (i+1) z)
{-# INLINE foldr #-}

undefinedElem :: a
undefinedElem = error "Undefined element!"

thaw :: Array e -> Int -> Int -> ST s (MArray s e)
thaw !ary !_o@(I# o#) !n@(I# n#) =
    CHECK_BOUNDS("thaw", length ary, _o + n)
        ST $ \ s -> case thawArray# (unArray ary) o# n# s of
            (# s2, mary# #) -> (# s2, marray mary# n #)
{-# INLINE thaw #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
delete :: Array e -> Int -> Array e
delete ary idx =
    run $ do
        mary <- new (count-1) undefinedElem
        copy ary 0 mary 0 idx
        copy ary (idx+1) mary idx (count-(idx+1))
        return mary
  where !count = length ary
{-# INLINE delete #-}

map :: (a -> b) -> Array a -> Array b
map f = \ ary ->
    let !n = length ary
    in run $ do
        mary <- new n undefinedElem
        go ary mary 0 n
  where
    go ary mary i n
        | i >= n    = return mary
        | otherwise = do
             write mary i $ f (index ary i)
             go ary mary (i+1) n
{-# INLINE map #-}
