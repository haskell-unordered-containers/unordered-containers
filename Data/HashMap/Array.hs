{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}

-- | Zero based arrays.
--
-- Note that no bounds checking are performed.
module Data.HashMap.Array
    ( SmallArray
    , SmallMutableArray

      -- * Creation
    , new
    , new_
    , singleton
    , singletonM
    , pair

      -- * Basic interface
    , sizeofSmallArray
    , sizeofSmallMutableArray
    , readSmallArray
    , writeSmallArray
    , indexSmallArray
    , indexSmallArrayM
    , indexSmallArray##
    , update
    , updateWith'
    , unsafeUpdateM
    , insert
    , insertM
    , delete
    , sameArray1
    , trim

    , unsafeFreezeSmallArray
    , unsafeThawSmallArray
    , unsafeSameArray
    , run
    , run2
    , copySmallArray
    , copySmallMutableArray

      -- * Folds
    , foldl'
    , foldr

    , thawSmallArray
    , map
    , mapSmallArray'
    , traverse
    , traverse'
    , toList
    , fromList
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif
import Control.Applicative (liftA2)
import Control.DeepSeq
import GHC.Exts(Int(..), Int#, reallyUnsafePtrEquality#, tagToEnum#, unsafeCoerce#, State#)
import GHC.ST (ST(..))
import Control.Monad.ST (stToIO)

#if __GLASGOW_HASKELL__ >= 709
import Prelude hiding (filter, foldr, length, map, read, traverse)
#else
import Prelude hiding (filter, foldr, length, map, read)
#endif

#if __GLASGOW_HASKELL__ >= 710
import GHC.Exts (SmallArray#, newSmallArray#, readSmallArray#, writeSmallArray#,
                 indexSmallArray#, unsafeFreezeSmallArray#, unsafeThawSmallArray#,
                 SmallMutableArray#, sizeofSmallArray#, copySmallArray#, thawSmallArray#,
                 sizeofSmallMutableArray#, copySmallMutableArray#, cloneSmallMutableArray#)

#else
import GHC.Exts (Array#, newArray#, readArray#, writeArray#,
                 indexArray#, unsafeFreezeArray#, unsafeThawArray#,
                 MutableArray#, sizeofArray#, copyArray#, thawArray#,
                 sizeofMutableArray#, copyMutableArray#, cloneMutableArray#)
#endif

#if defined(ASSERTS)
import qualified Prelude
#endif

import Data.HashMap.Unsafe (runST)
import Control.Monad ((>=>))


#if __GLASGOW_HASKELL__ >= 710
type Array# a = SmallArray# a
type MutableArray# a = SmallMutableArray# a

newArray# :: Int# -> a -> State# d -> (# State# d, SmallMutableArray# d a #)
newArray# = newSmallArray#

unsafeFreezeArray# :: SmallMutableArray# d a
                   -> State# d -> (# State# d, SmallArray# a #)
unsafeFreezeArray# = unsafeFreezeSmallArray#

readArray# :: SmallMutableArray# d a
           -> Int# -> State# d -> (# State# d, a #)
readArray# = readSmallArray#

writeArray# :: SmallMutableArray# d a
            -> Int# -> a -> State# d -> State# d
writeArray# = writeSmallArray#

indexArray# :: SmallArray# a -> Int# -> (# a #)
indexArray# = indexSmallArray#

unsafeThawArray# :: SmallArray# a
                 -> State# d -> (# State# d, SmallMutableArray# d a #)
unsafeThawArray# = unsafeThawSmallArray#

sizeofArray# :: SmallArray# a -> Int#
sizeofArray# = sizeofSmallArray#

copyArray# :: SmallArray# a
           -> Int#
           -> SmallMutableArray# d a
           -> Int#
           -> Int#
           -> State# d
           -> State# d
copyArray# = copySmallArray#

cloneMutableArray# :: SmallMutableArray# s a
                   -> Int#
                   -> Int#
                   -> State# s
                   -> (# State# s, SmallMutableArray# s a #)
cloneMutableArray# = cloneSmallMutableArray#

thawArray# :: SmallArray# a
           -> Int#
           -> Int#
           -> State# d
           -> (# State# d, SmallMutableArray# d a #)
thawArray# = thawSmallArray#

sizeofMutableArray# :: SmallMutableArray# s a -> Int#
sizeofMutableArray# = sizeofSmallMutableArray#

copyMutableArray# :: SmallMutableArray# d a
                  -> Int#
                  -> SmallMutableArray# d a
                  -> Int#
                  -> Int#
                  -> State# d
                  -> State# d
copyMutableArray# = copySmallMutableArray#
#endif

------------------------------------------------------------------------

#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
# define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("Data.HashMap.Array." ++ (_func_) ++ ": bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else
# define CHECK_OP(_func_,_op_,_lhs_,_rhs_) \
if not ((_lhs_) _op_ (_rhs_)) then error ("Data.HashMap.Array." ++ (_func_) ++ ": Check failed: _lhs_ _op_ _rhs_ (" ++ show (_lhs_) ++ " vs. " ++ show (_rhs_) ++ ")") else
# define CHECK_GT(_func_,_lhs_,_rhs_) CHECK_OP(_func_,>,_lhs_,_rhs_)
# define CHECK_LE(_func_,_lhs_,_rhs_) CHECK_OP(_func_,<=,_lhs_,_rhs_)
# define CHECK_EQ(_func_,_lhs_,_rhs_) CHECK_OP(_func_,==,_lhs_,_rhs_)
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
# define CHECK_OP(_func_,_op_,_lhs_,_rhs_)
# define CHECK_GT(_func_,_lhs_,_rhs_)
# define CHECK_LE(_func_,_lhs_,_rhs_)
# define CHECK_EQ(_func_,_lhs_,_rhs_)
#endif

data SmallArray a = SmallArray { unArray :: !(Array# a) }

instance Show a => Show (SmallArray a) where
    show = show . toList

-- Determines whether two arrays have the same memory address.
-- This is more reliable than testing pointer equality on the
-- Array wrappers, but it's still slightly bogus.
unsafeSameArray :: SmallArray a -> SmallArray b -> Bool
unsafeSameArray (SmallArray xs) (SmallArray ys) =
  tagToEnum# (unsafeCoerce# reallyUnsafePtrEquality# xs ys)

sameArray1 :: (a -> b -> Bool) -> SmallArray a -> SmallArray b -> Bool
sameArray1 eq !xs0 !ys0
  | lenxs /= lenys = False
  | otherwise = go 0 xs0 ys0
  where
    go !k !xs !ys
      | k == lenxs = True
      | (# x #) <- indexSmallArray## xs k
      , (# y #) <- indexSmallArray## ys k
      = eq x y && go (k + 1) xs ys

    !lenxs = sizeofSmallArray xs0
    !lenys = sizeofSmallArray ys0

sizeofSmallArray :: SmallArray a -> Int
sizeofSmallArray ary = I# (sizeofArray# (unArray ary))
{-# INLINE sizeofSmallArray #-}

-- | Smart constructor
array :: Array# a -> Int -> SmallArray a
array ary _n = SmallArray ary
{-# INLINE array #-}

data SmallMutableArray s a = SmallMutableArray {
      unMArray :: !(MutableArray# s a)
    }

sizeofSmallMutableArray :: SmallMutableArray s a -> Int
sizeofSmallMutableArray mary = I# (sizeofMutableArray# (unMArray mary))
{-# INLINE sizeofSmallMutableArray #-}

-- | Smart constructor
marray :: MutableArray# s a -> Int -> SmallMutableArray s a
marray mary _n = SmallMutableArray mary
{-# INLINE marray #-}

------------------------------------------------------------------------

instance NFData a => NFData (SmallArray a) where
    rnf = rnfArray

rnfArray :: NFData a => SmallArray a -> ()
rnfArray ary0 = go ary0 n0 0
  where
    n0 = sizeofSmallArray ary0
    go !ary !n !i
        | i >= n = ()
        | (# x #) <- indexSmallArray## ary i
        = rnf x `seq` go ary n (i+1)
-- We use indexSmallArray## just in case GHC can't see that the
-- relevant rnf is strict, or in case it actually isn't.
{-# INLINE rnfArray #-}

-- | Create a new mutable array of specified size, in the specified
-- state thread, with each element containing the specified initial
-- value.
new :: Int -> a -> ST s (SmallMutableArray s a)
new n@(I# n#) b =
    CHECK_GT("new",n,(0 :: Int))
    ST $ \s ->
        case newArray# n# b s of
            (# s', ary #) -> (# s', marray ary n #)
{-# INLINE new #-}

new_ :: Int -> ST s (SmallMutableArray s a)
new_ n = new n undefinedElem

singleton :: a -> SmallArray a
singleton x = runST (singletonM x)
{-# INLINE singleton #-}

singletonM :: a -> ST s (SmallArray a)
singletonM x = new 1 x >>= unsafeFreezeSmallArray
{-# INLINE singletonM #-}

pair :: a -> a -> SmallArray a
pair x y = run $ do
    ary <- new 2 x
    writeSmallArray ary 1 y
    return ary
{-# INLINE pair #-}

readSmallArray :: SmallMutableArray s a -> Int -> ST s a
readSmallArray ary _i@(I# i#) = ST $ \ s ->
    CHECK_BOUNDS("readSmallArray", sizeofSmallMutableArray ary, _i)
        readArray# (unMArray ary) i# s
{-# INLINE readSmallArray #-}

writeSmallArray :: SmallMutableArray s a -> Int -> a -> ST s ()
writeSmallArray ary _i@(I# i#) b = ST $ \ s ->
    CHECK_BOUNDS("writeSmallArray", sizeofSmallMutableArray ary, _i)
        case writeArray# (unMArray ary) i# b s of
            s' -> (# s' , () #)
{-# INLINE writeSmallArray #-}

indexSmallArray :: SmallArray a -> Int -> a
indexSmallArray ary _i@(I# i#) =
    CHECK_BOUNDS("indexSmallArray", sizeofSmallArray ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> b
{-# INLINE indexSmallArray #-}

indexSmallArray## :: SmallArray a -> Int -> (# a #)
indexSmallArray## ary _i@(I# i#) =
    CHECK_BOUNDS("indexSmallArray##", sizeofSmallArray ary, _i)
        indexArray# (unArray ary) i#
{-# INLINE indexSmallArray## #-}

indexSmallArrayM :: SmallArray a -> Int -> ST s a
indexSmallArrayM ary _i@(I# i#) =
    CHECK_BOUNDS("indexSmallArrayM", sizeofSmallArray ary, _i)
        case indexArray# (unArray ary) i# of (# b #) -> return b
{-# INLINE indexSmallArrayM #-}

unsafeFreezeSmallArray :: SmallMutableArray s a -> ST s (SmallArray a)
unsafeFreezeSmallArray mary
    = ST $ \s -> case unsafeFreezeArray# (unMArray mary) s of
                   (# s', ary #) -> (# s', array ary (sizeofSmallMutableArray mary) #)
{-# INLINE unsafeFreezeSmallArray #-}

unsafeThawSmallArray :: SmallArray a -> ST s (SmallMutableArray s a)
unsafeThawSmallArray ary
    = ST $ \s -> case unsafeThawArray# (unArray ary) s of
                   (# s', mary #) -> (# s', marray mary (sizeofSmallArray ary) #)
{-# INLINE unsafeThawSmallArray #-}

run :: (forall s . ST s (SmallMutableArray s e)) -> SmallArray e
run act = runST $ act >>= unsafeFreezeSmallArray
{-# INLINE run #-}

run2 :: (forall s. ST s (SmallMutableArray s e, a)) -> (SmallArray e, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreezeSmallArray marr
                 return (arr,b))

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copySmallArray :: SmallMutableArray s e -> Int -> SmallArray e -> Int -> Int -> ST s ()
copySmallArray !dst !_didx@(I# didx#) !src !_sidx@(I# sidx#) _n@(I# n#) =
    CHECK_LE("copySmallArray", _sidx + _n, sizeofSmallArray src)
    CHECK_LE("copySmallArray", _didx + _n, sizeofSmallMutableArray dst)
        ST $ \ s# ->
        case copyArray# (unArray src) sidx# (unMArray dst) didx# n# s# of
            s2 -> (# s2, () #)

-- | Unsafely copy the elements of an array. Array bounds are not checked.
copySmallMutableArray :: SmallMutableArray s e -> Int -> SmallMutableArray s e -> Int -> Int -> ST s ()
copySmallMutableArray !src !_sidx@(I# sidx#) !dst !_didx@(I# didx#) _n@(I# n#) =
    CHECK_BOUNDS("copySmallMutableArray: src", sizeofSmallMutableArray src, _sidx + _n - 1)
    CHECK_BOUNDS("copySmallMutableArray: dst", sizeofSmallMutableArray dst, _didx + _n - 1)
    ST $ \ s# ->
    case copyMutableArray# (unMArray src) sidx# (unMArray dst) didx# n# s# of
        s2 -> (# s2, () #)

cloneSmallMutableArray :: SmallMutableArray s a -> Int -> Int -> ST s (SmallMutableArray s a)
cloneSmallMutableArray _mary@(SmallMutableArray mary#) _off@(I# off#) _len@(I# len#) =
    CHECK_BOUNDS("cloneM_off", sizeofSmallMutableArray _mary, _off - 1)
    CHECK_BOUNDS("cloneM_end", sizeofSmallMutableArray _mary, _off + _len - 1)
    ST $ \ s ->
    case cloneMutableArray# mary# off# len# s of
      (# s', mary'# #) -> (# s', SmallMutableArray mary'# #)

-- | Create a new array of the @n@ first elements of @mary@.
trim :: SmallMutableArray s a -> Int -> ST s (SmallArray a)
trim mary n = cloneSmallMutableArray mary 0 n >>= unsafeFreezeSmallArray
{-# INLINE trim #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insert :: SmallArray e -> Int -> e -> SmallArray e
insert ary idx b = runST (insertM ary idx b)
{-# INLINE insert #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: SmallArray e -> Int -> e -> ST s (SmallArray e)
insertM ary idx b =
    CHECK_BOUNDS("insertM", count + 1, idx)
        do mary <- new_ (count+1)
           copySmallArray mary 0 ary 0 idx
           writeSmallArray mary idx b
           copySmallArray mary (idx+1) ary idx (count-idx)
           unsafeFreezeSmallArray mary
  where !count = sizeofSmallArray ary
{-# INLINE insertM #-}

-- | /O(n)/ Update the element at the given position in this array.
update :: SmallArray e -> Int -> e -> SmallArray e
update ary idx b = runST (updateM ary idx b)
{-# INLINE update #-}

-- | /O(n)/ Update the element at the given position in this array.
updateM :: SmallArray e -> Int -> e -> ST s (SmallArray e)
updateM ary idx b =
    CHECK_BOUNDS("updateM", count, idx)
        do mary <- thawSmallArray ary 0 count
           writeSmallArray mary idx b
           unsafeFreezeSmallArray mary
  where !count = sizeofSmallArray ary
{-# INLINE updateM #-}

-- | /O(n)/ Update the element at the given positio in this array, by
-- applying a function to it.  Evaluates the element to WHNF before
-- inserting it into the array.
updateWith' :: SmallArray e -> Int -> (e -> e) -> SmallArray e
updateWith' ary idx f
  | (# x #) <- indexSmallArray## ary idx
  = update ary idx $! f x
{-# INLINE updateWith' #-}

-- | /O(1)/ Update the element at the given position in this array,
-- without copying.
unsafeUpdateM :: SmallArray e -> Int -> e -> ST s ()
unsafeUpdateM ary idx b =
    CHECK_BOUNDS("unsafeUpdateM", sizeofSmallArray ary, idx)
        do mary <- unsafeThawSmallArray ary
           writeSmallArray mary idx b
           _ <- unsafeFreezeSmallArray mary
           return ()
{-# INLINE unsafeUpdateM #-}

foldl' :: (b -> a -> b) -> b -> SmallArray a -> b
foldl' f = \ z0 ary0 -> go ary0 (sizeofSmallArray ary0) 0 z0
  where
    go ary n i !z
        | i >= n = z
        | otherwise
        = case indexSmallArray## ary i of
            (# x #) -> go ary n (i+1) (f z x)
{-# INLINE foldl' #-}

foldr :: (a -> b -> b) -> b -> SmallArray a -> b
foldr f = \ z0 ary0 -> go ary0 (sizeofSmallArray ary0) 0 z0
  where
    go ary n i z
        | i >= n = z
        | otherwise
        = case indexSmallArray## ary i of
            (# x #) -> f x (go ary n (i+1) z)
{-# INLINE foldr #-}

undefinedElem :: a
undefinedElem = error "Data.HashMap.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

thawSmallArray :: SmallArray e -> Int -> Int -> ST s (SmallMutableArray s e)
thawSmallArray !ary !_o@(I# o#) !n@(I# n#) =
    CHECK_LE("thawSmallArray", _o + n, sizeofSmallArray ary)
        ST $ \ s -> case thawArray# (unArray ary) o# n# s of
            (# s2, mary# #) -> (# s2, marray mary# n #)
{-# INLINE thawSmallArray #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
delete :: SmallArray e -> Int -> SmallArray e
delete ary idx = runST (deleteM ary idx)
{-# INLINE delete #-}

-- | /O(n)/ Delete an element at the given position in this array,
-- decreasing its size by one.
deleteM :: SmallArray e -> Int -> ST s (SmallArray e)
deleteM ary idx = do
    CHECK_BOUNDS("deleteM", count, idx)
        do mary <- new_ (count-1)
           copySmallArray mary 0 ary 0 idx
           copySmallArray mary idx ary (idx+1) (count-(idx+1))
           unsafeFreezeSmallArray mary
  where !count = sizeofSmallArray ary
{-# INLINE deleteM #-}

map :: (a -> b) -> SmallArray a -> SmallArray b
map f = \ ary ->
    let !n = sizeofSmallArray ary
    in run $ do
        mary <- new_ n
        go ary mary 0 n
  where
    go ary mary i n
        | i >= n    = return mary
        | otherwise = do
             x <- indexSmallArrayM ary i
             writeSmallArray mary i $ f x
             go ary mary (i+1) n
{-# INLINE map #-}

-- | Strict version of 'map'.
mapSmallArray' :: (a -> b) -> SmallArray a -> SmallArray b
mapSmallArray' f = \ ary ->
    let !n = sizeofSmallArray ary
    in run $ do
        mary <- new_ n
        go ary mary 0 n
  where
    go ary mary i n
        | i >= n    = return mary
        | otherwise = do
             x <- indexSmallArrayM ary i
             writeSmallArray mary i $! f x
             go ary mary (i+1) n
{-# INLINE mapSmallArray' #-}

fromList :: Int -> [a] -> SmallArray a
fromList n xs0 =
    CHECK_EQ("fromList", n, Prelude.length xs0)
        run $ do
            mary <- new_ n
            go xs0 mary 0
  where
    go [] !mary !_   = return mary
    go (x:xs) mary i = do writeSmallArray mary i x
                          go xs mary (i+1)

toList :: SmallArray a -> [a]
toList = foldr (:) []

newtype STA a = STA {_runSTA :: forall s. MutableArray# s a -> ST s (SmallArray a)}

runSTA :: Int -> STA a -> SmallArray a
runSTA !n (STA m) = runST $ new_ n >>= \ (SmallMutableArray ar) -> m ar

traverse :: Applicative f => (a -> f b) -> SmallArray a -> f (SmallArray b)
traverse f = \ !ary ->
  let
    !len = sizeofSmallArray ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezeSmallArray (SmallMutableArray mary)
      | (# x #) <- indexSmallArray## ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writeSmallArray (SmallMutableArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in runSTA len <$> go 0
{-# INLINE [1] traverse #-}

-- TODO: Would it be better to just use a lazy traversal
-- and then force the elements of the result? My guess is
-- yes.
traverse' :: Applicative f => (a -> f b) -> SmallArray a -> f (SmallArray b)
traverse' f = \ !ary ->
  let
    !len = sizeofSmallArray ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezeSmallArray (SmallMutableArray mary)
      | (# x #) <- indexSmallArray## ary i
      = liftA2 (\ !b (STA m) -> STA $ \mary ->
                    writeSmallArray (SmallMutableArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in runSTA len <$> go 0
{-# INLINE [1] traverse' #-}

-- Traversing in ST, we don't need to get fancy; we
-- can just do it directly.
traverseST :: (a -> ST s b) -> SmallArray a -> ST s (SmallArray b)
traverseST f = \ ary0 ->
  let
    !len = sizeofSmallArray ary0
    go k !mary
      | k == len = return mary
      | otherwise = do
          x <- indexSmallArrayM ary0 k
          y <- f x
          writeSmallArray mary k y
          go (k + 1) mary
  in new_ len >>= (go 0 >=> unsafeFreezeSmallArray)
{-# INLINE traverseST #-}

traverseIO :: (a -> IO b) -> SmallArray a -> IO (SmallArray b)
traverseIO f = \ ary0 ->
  let
    !len = sizeofSmallArray ary0
    go k !mary
      | k == len = return mary
      | otherwise = do
          x <- stToIO $ indexSmallArrayM ary0 k
          y <- f x
          stToIO $ writeSmallArray mary k y
          go (k + 1) mary
  in stToIO (new_ len) >>= (go 0 >=> stToIO . unsafeFreezeSmallArray)
{-# INLINE traverseIO #-}


-- Why don't we have similar RULES for traverse'? The efficient
-- way to traverse strictly in IO or ST is to force results as
-- they come in, which leads to different semantics. In particular,
-- we need to ensure that
--
--  traverse' (\x -> print x *> pure undefined) xs
--
-- will actually print all the values and then return undefined.
-- We could add a strict mapMWithIndex, operating in an arbitrary
-- Monad, that supported such rules, but we don't have that right now.
{-# RULES
"traverse/ST" forall f. traverse f = traverseST f
"traverse/IO" forall f. traverse f = traverseIO f
 #-}
