{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable #-}

module Data.HashMap.Base
    (
      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , lookup
    , insert
    , delete

      -- * Combine
      -- * Union
    , union

      -- * Transformations
    , map

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
    ) where

import Control.DeepSeq (NFData(rnf))
import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.), complement)
import qualified Data.List as L
import Data.Word (Word)
import Prelude hiding (filter, foldr, lookup, map, null)

import qualified Data.HashMap.Array as A
import qualified Data.Hashable as H
import Data.Hashable (Hashable)
import Data.HashMap.PopCount (popCount)
import Data.HashMap.UnsafeShift (unsafeShiftL, unsafeShiftR)
import Data.Typeable (Typeable)

------------------------------------------------------------------------

-- | Convenience function.  Compute a hash value for the given value.
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

-- TODO: Shouldn't contain hash.
data Leaf k v = L {-# UNPACK #-} !Hash !k v

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L _ k v) = rnf k `seq` rnf v

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Empty
    | BitmapIndexed {-# UNPACK #-} !Bitmap {-# UNPACK #-} !(A.Array (HashMap k v))
    | Leaf {-# UNPACK #-} !(Leaf k v)
    | Full {-# UNPACK #-} !(A.Array (HashMap k v))
    | Collision {-# UNPACK #-} !Hash {-# UNPACK #-} !(A.Array (Leaf k v))
      deriving (Typeable)

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf (L _ k v))      = rnf k `seq` rnf v
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

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
singleton k v = Leaf (L (hash k) k v)

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
    go (Leaf _)              n = n + 1
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
    go h k _ (Leaf (L hx kx x))
        | h == hx && k == kx = Just x
        | otherwise = Nothing
    go h k s (BitmapIndexed b v) =
        let m = bitpos h s
        in if b .&. m == 0
           then Nothing
           else go h k (s+bitsPerSubkey) (A.index v (index b m))
    go h k s (Full v) = go h k (s+bitsPerSubkey) (A.index v (mask h s))
    go h k _ (Collision hx v)
        | h == hx   = lookupInArray h k v
        | otherwise = Nothing
{-# INLINABLE lookup #-}

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
                        ary' = A.insert ary i $! l
                        b'   = b .|. m
                    in if b' == fullNodeMask
                       then Full ary'
                       else BitmapIndexed b' ary'
               else let  st   = A.index ary i
                         st'  = go h k x (s+bitsPerSubkey) st
                         ary' = A.update ary i $! st'
                    in BitmapIndexed b ary'
    go h k x s (Full ary) =
        let i    = mask h s
            st   = A.index ary i
            st'  = go h k x (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
    go h k x s t@(Collision hy v)
        | h == hy = Collision h (updateOrSnoc h k x v)
        | otherwise = go h k x s $ BitmapIndexed (bitpos hy s) (A.singleton t)
{-# INLINABLE insert #-}

delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k0 = go h0 k0 0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf (L hy ky _))
        | hy == h && ky == k = Empty
        | otherwise = t
    go h k s t@(BitmapIndexed b ary) =
        let m = bitpos h s
        in if b .&. m == 0
               then t
               else let i    = index b m
                        st   = A.index ary i
                        st'  = go h k (s+bitsPerSubkey) st
                    in case st' of
                            Empty -> let ary' = A.delete ary i
                                     in BitmapIndexed (b .&. complement m) ary'
                            _ -> let ary' = A.update ary i $! st'
                                 in BitmapIndexed b ary'
    go h k s (Full ary) =
        let i    = mask h s
            st   = A.index ary i
            st'  = go h k (s+bitsPerSubkey) st
        in case st' of
            Empty -> let ary' = A.delete ary i
                     in BitmapIndexed (bitpos h s) ary'
            _ -> let ary' = A.update ary i $! st'
                 in Full ary'
    go h k _ t@(Collision hy v)
        | h == hy = case indexOf h k v of
            Just i
                | A.length v == 2 ->
                    if i == 0
                    then Leaf (A.index v 1)
                    else Leaf (A.index v 0)
                | otherwise -> Collision h (A.delete v i)
            Nothing -> t
        | otherwise = t
{-# INLINABLE delete #-}

------------------------------------------------------------------------
-- * Combine

-- | /O(n+m)/ The union of two maps. If a key occurs in both maps,
-- the mapping from the first will be the mapping in the result.
union :: (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
union m1 m2 = foldlWithKey' (\ m k v -> insert k v m) m2 m1

------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = go
  where
    go Empty = Empty
    go (Leaf (L h k v)) = Leaf $ L h k (f v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map go ary
    go (Full ary) = Full $ A.map go ary
    go (Collision h ary) = Collision h $
                           A.map (\ (L h' k v) -> L h' k (f v)) ary
{-# INLINE map #-}

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
    go !z Empty = z
    go z (Leaf (L _ k v)) = f z k v
    go z (BitmapIndexed _ ary) = A.foldl' go z ary
    go z (Full ary) = A.foldl' go z ary
    go z (Collision _ ary) = A.foldl' (\ z' (L _ k v) -> f z' k v) z ary
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
    go z Empty = z
    go z (Leaf (L _ k v)) = f k v z
    go z (BitmapIndexed _ ary) = A.foldr (flip go) z ary
    go z (Full ary) = A.foldr (flip go) z ary
    go z (Collision _ ary) = A.foldr (\ (L _ k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey #-}

------------------------------------------------------------------------
-- * Filter

-- Helper for filtering a sparse array.
filterA :: (a -> Bool) -> A.Array a -> Bitmap -> (A.Array a, Bitmap)
filterA p = \ary b0 ->
    let !n = A.length ary
    in A.run2 $ do
        mary <- A.new_ n
        go ary mary b0 0 0 0 n
  where
    go !ary !mary !b i !j !bi n
        | i >= n || bi > 1 `unsafeShiftL` n =  -- TODO: Verify
            if i == j
            then return (mary, b)
            else do mary2 <- A.new j undefinedElem
                    A.copyM mary 0 mary2 0 j
                    return (mary2, b)
        | bi .&. b == 0 = go ary mary b i j (bi `unsafeShiftL` 1) n
        | otherwise =
            let el = A.index ary i
            in if p el
               then do A.write mary j el
                       go ary mary b (i+1) (j+1) (bi `unsafeShiftL` 1) n
               else go ary mary (b .&. complement bi) (i+1) j
                    (bi `unsafeShiftL` 1) n

-- | /O(n)/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: (k -> v -> Bool) -> HashMap k v -> HashMap k v
filterWithKey pred = go
  where
    go Empty = Empty
    go t@(Leaf (L _ k v))
        | pred k v = t
        | otherwise = Empty
    go (BitmapIndexed b ary) =
        let (ary', b2) = filterEmpty ary b
            n          = A.length ary'
        in case n of
            0 -> Empty
            1 -> A.index ary' 0
            _ -> BitmapIndexed b2 ary'
    go (Full ary) =
        let (ary', b) = filterEmpty ary fullNodeMask
            n         = A.length ary'
        in case n of
            0 -> Empty
            1 -> A.index ary' 0
            _ | fromIntegral n == 1 `unsafeShiftL` bitsPerSubkey -> Full ary'
              | otherwise -> BitmapIndexed b ary'
    go (Collision h ary) = let ary' = A.filter (\ (L _ k v) -> pred k v) ary
                           in case A.length ary' of
                               0 -> Empty
                               1 -> Leaf $ A.index ary' 0
                               _ -> Collision h ary'

    isEmpty Empty = True
    isEmpty _     = False

    filterEmpty = filterA (not . isEmpty . go)  -- Reduces code duplication
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
toList = foldrWithKey (\ k v xs -> (k, v) : xs) []
{-# INLINABLE toList #-}

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = L.foldl' (\ m (k, v) -> insert k v m) empty
{-# INLINABLE fromList #-}

------------------------------------------------------------------------
-- Array operations

-- | /O(n)/ Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
lookupInArray :: Eq k => Hash -> k -> A.Array (Leaf k v) -> Maybe v
lookupInArray h0 k0 ary0 = go h0 k0 ary0 0 (A.length ary0)
  where
    go !h !k !ary !i !n
        | i >= n = Nothing
        | otherwise = case A.index ary i of
            (L hx kx v)
                | h == hx && k == kx -> Just v
                | otherwise -> go h k ary (i+1) n
{-# INLINABLE lookupInArray #-}

-- | /O(n)/ Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
indexOf :: Eq k => Hash -> k -> A.Array (Leaf k v) -> Maybe Int
indexOf h0 k0 ary0 = go h0 k0 ary0 0 (A.length ary0)
  where
    go !h !k !ary !i !n
        | i >= n = Nothing
        | otherwise = case A.index ary i of
            (L hx kx _)
                | h == hx && k == kx -> Just i
                | otherwise -> go h k ary (i+1) n
{-# INLINABLE indexOf #-}

updateOrSnoc :: Eq k => Hash -> k -> v -> A.Array (Leaf k v)
             -> A.Array (Leaf k v)
updateOrSnoc h0 k0 v0 ary0 = go h0 k0 v0 ary0 0 (A.length ary0)
  where
    go !h !k v !ary !i !n
        | i >= n = A.run $ do
            -- Not found, append to the end.
            mary <- A.new (n + 1) undefinedElem
            A.copy ary 0 mary 0 n
            A.write mary n (L h k v)
            return mary
        | otherwise = case A.index ary i of
            (L hx kx _)
                | h == hx && k == kx -> A.update ary i (L h k v)
                | otherwise -> go h k v ary (i+1) n
{-# INLINABLE updateOrSnoc #-}

undefinedElem :: a
undefinedElem = error "Undefined element!"

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
#if __GLASGOW_HASKELL__ >= 701
    A.thaw ary 0 16
#else
    do mary <- new 16 undefinedElem
       A.indexM ary 0 >>= A.write mary 0
       A.indexM ary 1 >>= A.write mary 1
       A.indexM ary 2 >>= A.write mary 2
       A.indexM ary 3 >>= A.write mary 3
       A.indexM ary 4 >>= A.write mary 4
       A.indexM ary 5 >>= A.write mary 5
       A.indexM ary 6 >>= A.write mary 6
       A.indexM ary 7 >>= A.write mary 7
       A.indexM ary 8 >>= A.write mary 8
       A.indexM ary 9 >>= A.write mary 9
       A.indexM ary 10 >>= A.write mary 10
       A.indexM ary 11 >>= A.write mary 11
       A.indexM ary 12 >>= A.write mary 12
       A.indexM ary 13 >>= A.write mary 13
       A.indexM ary 14 >>= A.write mary 14
       A.indexM ary 15 >>= A.write mary 15
       return mary
#endif

------------------------------------------------------------------------
-- Bit twiddling

bitsPerSubkey :: Int
bitsPerSubkey = 4

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

index :: Bitmap -> Bitmap -> Int
index b m = popCount (b .&. (m - 1))

bitpos :: Word -> Shift -> Bitmap
bitpos h s = 1 `unsafeShiftL` mask h s
{-# INLINE bitpos #-}

mask :: Word -> Shift -> Int
mask h s = fromIntegral $ unsafeShiftR h s .&. subkeyMask
{-# INLINE mask #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL`
                           fromIntegral (1 `unsafeShiftL` bitsPerSubkey))
{-# INLINE fullNodeMask #-}
