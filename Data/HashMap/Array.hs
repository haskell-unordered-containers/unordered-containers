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

      -- * Forcing
    , rnfArray
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif
import Control.Applicative (liftA2)
import Control.DeepSeq
import GHC.Exts(Int(..), Int#, reallyUnsafePtrEquality#, tagToEnum#, unsafeCoerce#, State#)
import GHC.ST (ST(..))
import Control.Monad.ST (stToIO)
import Data.Primitive.SmallArray
import qualified Data.Traversable

#if __GLASGOW_HASKELL__ >= 709
import Prelude hiding (filter, foldr, length, map, read, traverse)
#else
import Prelude hiding (filter, foldr, length, map, read)
#endif

#if defined(ASSERTS)
import qualified Prelude
#endif

import Data.HashMap.Unsafe (runST)
import Control.Monad ((>=>))

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

------------------------------------------------------------------------

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
new = newSmallArray

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

run :: (forall s . ST s (SmallMutableArray s e)) -> SmallArray e
run act = runST $ act >>= unsafeFreezeSmallArray
{-# INLINE run #-}

run2 :: (forall s. ST s (SmallMutableArray s e, a)) -> (SmallArray e, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreezeSmallArray marr
                 return (arr,b))

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

traverse :: Applicative f => (a -> f b) -> SmallArray a -> f (SmallArray b)
traverse = Data.Traversable.traverse

-- definitely fix this
traverse' :: Applicative f => (a -> f b) -> SmallArray a -> f (SmallArray b)
traverse' = Data.Traversable.traverse

