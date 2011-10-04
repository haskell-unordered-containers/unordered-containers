{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable #-}

module Data.HashMap.Base
    (
      HashMap(..)
    , Leaf(..)

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , lookup
    , lookupDefault
    , insert
    , insertWith
    , delete
    , adjust

      -- * Combine
      -- ** Union
    , union
    , unionWith

      -- * Transformations
    , map
    , traverseWithKey

      -- * Difference and intersection
    , difference
    , intersection

      -- * Folds
    , foldl'
    , foldlWithKey'
    , foldr
    , foldrWithKey

      -- * Filter
    , filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith

      -- Used by the strict version
    , collision
    , hash
    , bitpos
    , mask
    , bitsPerSubkey
    , fullNodeMask
    , index
    , update16
    ) where

import Control.Applicative ((<$>), Applicative(pure))
import Control.DeepSeq (NFData(rnf))
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.), (.|.), complement)
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import Data.Word (Word)
import Prelude hiding (filter, foldr, lookup, map, null, pred)

import qualified Data.HashMap.Array as A
import qualified Data.Hashable as H
import Data.Hashable (Hashable)
import Data.HashMap.PopCount (popCount)
import Data.HashMap.UnsafeShift (unsafeShiftL, unsafeShiftR)
import Data.Typeable (Typeable)

#if defined(__GLASGOW_HASKELL__)
import GHC.Exts (build)
#endif

------------------------------------------------------------------------

-- | Convenience function.  Compute a hash value for the given value.
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L !k v

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L k v) = rnf k `seq` rnf v

-- Invariant: The length of the 1st argument to 'Full' is
-- 2^bitsPerSubkey

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Empty
    | BitmapIndexed {-# UNPACK #-} !Bitmap {-# UNPACK #-} !(A.Array (HashMap k v))
    | Leaf {-# UNPACK #-} !Hash {-# UNPACK #-} !(Leaf k v)
    | Full {-# UNPACK #-} !(A.Array (HashMap k v))
    | Collision {-# UNPACK #-} !Hash {-# UNPACK #-} !(A.Array (Leaf k v))
      deriving (Typeable)

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf _ l)            = rnf l
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

instance Functor (HashMap k) where
    fmap = map

instance Foldable.Foldable (HashMap k) where
    foldr f = foldrWithKey (const f)

type Hash   = Word
type Bitmap = Word
type Shift  = Int

instance (Show k, Show v) => Show (HashMap k v) where
    show m = "fromList " ++ show (toList m)

-- NOTE: This is just a placeholder.
instance (Eq k, Eq v) => Eq (HashMap k v) where
    a == b = toList a == toList b

------------------------------------------------------------------------
-- * Construction

-- | /O(1)/ Construct an empty map.
empty :: HashMap k v
empty = Empty

-- | /O(1)/ Construct a map with a single element.
singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k v = Leaf (hash k) (L k v)

------------------------------------------------------------------------
-- * Basic interface

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: HashMap k v -> Bool
null Empty = True
null _   = False

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: HashMap k v -> Int
size t = go t 0
  where
    go Empty                !n = n
    go (Leaf _ _)            n = n + 1
    go (BitmapIndexed _ ary) n = A.foldl' (flip go) n ary
    go (Full ary)            n = A.foldl' (flip go) n ary
    go (Collision _ ary)     n = n + A.length ary

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Nothing
    go h k _ (Leaf hx (L kx x))
        | h == hx && k == kx = Just x
        | otherwise          = Nothing
    go h k s (BitmapIndexed b v)
        | b .&. m == 0 = Nothing
        | otherwise    = go h k (s+bitsPerSubkey) (A.index v (index b m))
      where m = bitpos h s
    go h k s (Full v) = go h k (s+bitsPerSubkey) (A.index v (mask h s))
    go h k _ (Collision hx v)
        | h == hx   = lookupInArray k v
        | otherwise = Nothing
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
lookupDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
lookupDefault def k t = case lookup k t of
    Just v -> v
    _      -> def
{-# INLINE lookupDefault #-}

-- | Create a 'Collision' value with two 'Leaf' values.
collision :: Hash -> Leaf k v -> Leaf k v -> HashMap k v
collision h e1 e2 =
    let v = A.run $ do mary <- A.new 2 e1
                       A.write mary 1 e2
                       return mary
    in Collision h v
{-# INLINE collision #-}

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert = insertWith' (\ new _old -> new)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
          -> HashMap k v
insertWith = insertWith'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

-- Always inlined to the implementation of 'insert' doesn't have to
-- pay the cost of using the higher-order argument.
insertWith' :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
            -> HashMap k v
insertWith' f k0 v0 = go h0 k0 v0 0
  where
    h0 = hash k0
    go !h !k x !_ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then Leaf h (L k (f x y))
                    else collision h l (L k x)
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
    go h k x s (BitmapIndexed b ary)
        | b .&. m == 0 = let l    = Leaf h (L k x)
                             ary' = A.insert ary i $! l
                             b'   = b .|. m
                         in if b' == fullNodeMask
                            then Full ary'
                            else BitmapIndexed b' ary'
        | otherwise = let  st   = A.index ary i
                           st'  = go h k x (s+bitsPerSubkey) st
                           ary' = A.update ary i $! st'
                      in BitmapIndexed b ary'
      where m = bitpos h s
            i = index b m
    go h k x s (Full ary) =
        let i    = mask h s
            st   = A.index ary i
            st'  = go h k x (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
    go h k x s t@(Collision hy v)
        | h == hy   = Collision h (updateOrSnocWith f k x v)
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
{-# INLINE insertWith' #-}

-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky _))
        | hy == h && ky == k = Empty
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise =
            let i   = index b m
                st  = A.index ary i
            in case go h k (s+bitsPerSubkey) st of
                Empty ->
                    if A.length ary == 1
                    then Empty
                    else BitmapIndexed (b .&. complement m) (A.delete ary i)
                st'   -> BitmapIndexed b (A.update ary i $! st')
      where m = bitpos h s
    go h k s (Full ary) =
        let i  = mask h s
            st = A.index ary i
        in case go h k (s+bitsPerSubkey) st of
            Empty -> BitmapIndexed (bitpos h s) (A.delete ary i)
            st'   -> Full (A.update ary i $! st')
    go h k _ t@(Collision hy v)
        | h == hy = case indexOf k v of
            Just i
                | A.length v == 2 ->
                    if i == 0
                    then Leaf h (A.index v 1)
                    else Leaf h (A.index v 0)
                | otherwise -> Collision h (A.delete v i)
            Nothing -> t
        | otherwise = t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky y))
        | hy == h && ky == k = Leaf h (L k (f y))
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise = let st   = A.index ary i
                          st'  = go h k (s+bitsPerSubkey) st
                          ary' = A.update ary i $! st'
                      in BitmapIndexed b ary'
      where m = bitpos h s
            i = index b m
    go h k s (Full ary) =
        let i    = mask h s
            st   = A.index ary i
            st'  = go h k (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
    go h k _ t@(Collision hy v)
        | h == hy   = Collision h (updateWith f k v)
        | otherwise = t
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#endif

------------------------------------------------------------------------
-- * Combine

-- | /O(n*log m)/ The union of two maps. If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union :: (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
union m1 m2 = foldlWithKey' (\ m k v -> insert k v m) m2 m1
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE union #-}
#endif

-- | /O(n*log m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWith f m1 m2 = foldlWithKey' (\ m k v -> insertWith f k v m) m2 m1
{-# INLINE unionWith #-}

------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go Empty = Empty
    go (Leaf h (L k v)) = Leaf h $ L k (f v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map' go ary
    go (Full ary) = Full $ A.map' go ary
    go (Collision h ary) = Collision h $
                           A.map' (\ (L k v) -> L k (f v)) ary
{-# INLINE map #-}

-- | /O(n)/ Transform this map by accumulating an Applicative result
-- from every value.
traverseWithKey :: Applicative f => (k -> v1 -> f v2) -> HashMap k v1
                -> f (HashMap k v2)
traverseWithKey f = go
  where
    go Empty                 = pure Empty
    go (Leaf h (L k v))      = Leaf h . L k <$> f k v
    go (BitmapIndexed b ary) = BitmapIndexed b <$> A.traverse go ary
    go (Full ary)            = Full <$> A.traverse go ary
    go (Collision h ary)     =
        Collision h <$> A.traverse (\ (L k v) -> L k <$> f k v) ary
{-# INLINE traverseWithKey #-}

------------------------------------------------------------------------
-- * Difference and intersection

-- | /O(n)/ Difference of two maps. Return elements of the first map
-- not existing in the second.
difference :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> insert k v m
                 _       -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE difference #-}
#endif

-- | /O(n)/ Intersection of two maps. Return elements of the first map
-- for keys existing in the second.
intersection :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
intersection a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just _ -> insert k v m
                 _      -> m
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE intersection #-}
#endif

------------------------------------------------------------------------
-- * Folds

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> HashMap k v -> a
foldl' f = foldlWithKey' (\ z _ v -> f z v)
{-# INLINE foldl' #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' f = go
  where
    go !z Empty                = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl' go z ary
    go z (Full ary)            = A.foldl' go z ary
    go z (Collision _ ary)     = A.foldl' (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey' #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> HashMap k v -> a
foldr f = foldrWithKey (const f)
{-# INLINE foldr #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f = go
  where
    go z Empty                 = z
    go z (Leaf _ (L k v))      = f k v z
    go z (BitmapIndexed _ ary) = A.foldr (flip go) z ary
    go z (Full ary)            = A.foldr (flip go) z ary
    go z (Collision _ ary)     = A.foldr (\ (L k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey #-}

------------------------------------------------------------------------
-- * Filter

-- | /O(n)/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: (k -> v -> Bool) -> HashMap k v -> HashMap k v
filterWithKey pred = go
  where
    go Empty = Empty
    go t@(Leaf _ (L k v))
        | pred k v  = t
        | otherwise = Empty
    go (BitmapIndexed b ary) = filterA ary b
    go (Full ary) = filterA ary fullNodeMask
    go (Collision h ary) = filterC ary h

    filterA ary0 b0 =
        let !n = A.length ary0
        in runST $ do
            mary <- A.new_ n
            step ary0 mary b0 0 0 1 n
      where
        step !ary !mary !b i !j !bi n
            | i >= n = case j of
                0 -> return Empty
                1 -> A.read mary 0
                _ -> do
                    mary2 <- A.new_ j
                    A.copyM mary 0 mary2 0 j
                    ary2 <- A.unsafeFreeze mary2
                    return $! if j == maxChildren
                              then Full ary2
                              else BitmapIndexed b ary2
            | bi .&. b == 0 = step ary mary b i j (bi `unsafeShiftL` 1) n
            | otherwise = case go (A.index ary i) of
                Empty -> step ary mary (b .&. complement bi) (i+1) j
                         (bi `unsafeShiftL` 1) n
                t     -> do A.write mary j t
                            step ary mary b (i+1) (j+1) (bi `unsafeShiftL` 1) n

    filterC ary0 h =
        let !n = A.length ary0
        in runST $ do
            mary <- A.new_ n
            step ary0 mary 0 0 n
      where
        step ary mary i j n
            | i >= n    = case j of
                0 -> return Empty
                1 -> do l <- A.read mary 0
                        return $! Leaf h l
                _ | i == j -> do ary2 <- A.unsafeFreeze mary
                                 return $! Collision h ary2
                  | otherwise -> do mary2 <- A.new_ j
                                    A.copyM mary 0 mary2 0 j
                                    ary2 <- A.unsafeFreeze mary2
                                    return $! Collision h ary2
            | pred k v  = A.write mary j el >> step ary mary (i+1) (j+1) n
            | otherwise = step ary mary (i+1) j n
          where el@(L k v) = A.index ary i
{-# INLINE filterWithKey #-}

-- | /O(n)/ Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (v -> Bool) -> HashMap k v -> HashMap k v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

------------------------------------------------------------------------
-- * Conversions

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: HashMap k v -> [k]
keys = L.map fst . toList
{-# INLINE keys #-}

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: HashMap k v -> [v]
elems = L.map snd . toList
{-# INLINE elems #-}

------------------------------------------------------------------------
-- ** Lists

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily.
toList :: HashMap k v -> [(k, v)]
#if defined(__GLASGOW_HASKELL__)
toList t = build (\ c z -> foldrWithKey (curry c) z t)
#else
toList = foldrWithKey (\ k v xs -> (k, v) : xs) []
#endif
{-# INLINE toList #-}

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = L.foldl' (\ m (k, v) -> insert k v m) empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromList #-}
#endif

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = L.foldl' (\ m (k, v) -> insertWith f k v m) empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINE fromListWith #-}
#endif

------------------------------------------------------------------------
-- Array operations

-- | /O(n)/ Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
lookupInArray :: Eq k => k -> A.Array (Leaf k v) -> Maybe v
lookupInArray k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = Nothing
        | otherwise = case A.index ary i of
            (L kx v)
                | k == kx   -> Just v
                | otherwise -> go k ary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookupInArray #-}
#endif

-- | /O(n)/ Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
indexOf :: Eq k => k -> A.Array (Leaf k v) -> Maybe Int
indexOf k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = Nothing
        | otherwise = case A.index ary i of
            (L kx _)
                | k == kx   -> Just i
                | otherwise -> go k ary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE indexOf #-}
#endif

updateWith :: Eq k => (v -> v) -> k -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateWith f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = ary
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> A.update ary i (L k (f y))
                     | otherwise -> go k ary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateWith #-}
#endif

updateOrSnocWith :: Eq k => (v -> v -> v) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWith f k0 v0 ary0 = go k0 v0 ary0 0 (A.length ary0)
  where
    go !k v !ary !i !n
        | i >= n = A.run $ do
            -- Not found, append to the end.
            mary <- A.new_ (n + 1)
            A.copy ary 0 mary 0 n
            A.write mary n (L k v)
            return mary
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> A.update ary i (L k (f v y))
                     | otherwise -> go k v ary (i+1) n
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE updateOrSnocWith #-}
#endif

------------------------------------------------------------------------
-- Manually unrolled loops

-- | /O(n)/ Update the element at the given position in this array.
update16 :: A.Array e -> Int -> e -> A.Array e
update16 ary idx b =
    A.run $ do
        mary <- clone16 ary
        A.write mary idx b
        return mary
{-# INLINE update16 #-}

-- | Unsafely clone an array of 16 elements.  The length of the input
-- array is not checked.
clone16 :: A.Array e -> ST s (A.MArray s e)
clone16 ary =
#if __GLASGOW_HASKELL__ >= 702
    A.thaw ary 0 16
#else
    do mary <- A.new_ 16
       A.index_ ary 0 >>= A.write mary 0
       A.index_ ary 1 >>= A.write mary 1
       A.index_ ary 2 >>= A.write mary 2
       A.index_ ary 3 >>= A.write mary 3
       A.index_ ary 4 >>= A.write mary 4
       A.index_ ary 5 >>= A.write mary 5
       A.index_ ary 6 >>= A.write mary 6
       A.index_ ary 7 >>= A.write mary 7
       A.index_ ary 8 >>= A.write mary 8
       A.index_ ary 9 >>= A.write mary 9
       A.index_ ary 10 >>= A.write mary 10
       A.index_ ary 11 >>= A.write mary 11
       A.index_ ary 12 >>= A.write mary 12
       A.index_ ary 13 >>= A.write mary 13
       A.index_ ary 14 >>= A.write mary 14
       A.index_ ary 15 >>= A.write mary 15
       return mary
#endif

------------------------------------------------------------------------
-- Bit twiddling

bitsPerSubkey :: Int
bitsPerSubkey = 4

maxChildren :: Int
maxChildren = fromIntegral $ 1 `unsafeShiftL` bitsPerSubkey

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

index :: Bitmap -> Bitmap -> Int
index b m = popCount (b .&. (m - 1))

bitpos :: Word -> Shift -> Bitmap
bitpos w s = 1 `unsafeShiftL` mask w s
{-# INLINE bitpos #-}

-- | Mask out the 'bitsPerSubkey' bits used for indexing at this level
-- of the tree.
mask :: Word -> Shift -> Int
mask w s = fromIntegral $ unsafeShiftR w s .&. subkeyMask
{-# INLINE mask #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL`
                           fromIntegral (1 `unsafeShiftL` bitsPerSubkey))
{-# INLINE fullNodeMask #-}
