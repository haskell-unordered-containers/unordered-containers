{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.UnorderedIntMap
    (
      UnorderedIntMap(..)
    , Leaf(..)

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , member
    , lookup
    , lookupDefault
    , (!)
    , insert
    , insertWith
    , unsafeInsert
    , delete
    , adjust
    , update
    , alter

      -- * Combine
      -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions

      -- * Transformations
    , map
    , mapWithKey
    , traverseWithKey

      -- * Difference and intersection
    , difference
    , differenceWith
    , intersection
    , intersectionWith
    , intersectionWithKey

      -- * Folds
    , foldl'
    , foldlWithKey'
    , foldr
    , foldrWithKey

      -- * Filter
    , mapMaybe
    , mapMaybeWithKey
    , filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith

      -- Internals used by the strict version
    , Bitmap
    , bitmapIndexedOrFull
    , mask
    , index
    , bitsPerSubkey
    , fullNodeMask
    , sparseIndex
    , two
    , unionArrayBy
    , update16
    , update16M
    , update16With'
    , updateOrConcatWith
    , updateOrConcatWithKey
    , filterMapAux
    , equalKeys
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), Applicative(pure))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (Traversable(..))
import Data.Word (Word)
#endif
#if __GLASGOW_HASKELL__ >= 711
import Data.Semigroup (Semigroup((<>)))
#endif
import Control.DeepSeq (NFData(rnf))
import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.), complement, popCount)
import Data.Data hiding (Typeable)
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import GHC.Exts ((==#), build, reallyUnsafePtrEquality#)
import Prelude hiding (filter, foldr, lookup, map, null, pred)
import Text.Read hiding (step)

import qualified Data.HashMap.Array as A
import Data.HashMap.Unsafe (runST)
import Data.HashMap.UnsafeShift (unsafeShiftL, unsafeShiftR)
import Data.Typeable (Typeable)

import GHC.Exts (isTrue#)
import qualified GHC.Exts as Exts

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif

-- | A set of values.  A set cannot contain duplicate values.
------------------------------------------------------------------------

data Leaf v = L {-# UNPACK #-} !Int v
  deriving (Eq)

instance NFData v => NFData (Leaf v) where
    rnf (L _ v) = rnf v

-- Invariant: The length of the 1st argument to 'Full' is
-- 2^bitsPerSubkey

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data UnorderedIntMap v
    = Empty
    | BitmapIndexed  {-# UNPACK #-} !Bitmap {-# UNPACK #-} !(A.Array (UnorderedIntMap v))
    | Leaf {-# UNPACK #-} !(Leaf v)
    | Full {-# UNPACK #-} !(A.Array (UnorderedIntMap v))
      deriving (Typeable)

type role UnorderedIntMap representational

instance NFData v => NFData (UnorderedIntMap v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf l)            = rnf l
    rnf (Full ary)            = rnf ary

instance Functor UnorderedIntMap where
    fmap = map

instance Foldable.Foldable UnorderedIntMap where
    foldr f = foldrWithKey (const f)

#if __GLASGOW_HASKELL__ >= 711
instance Semigroup (UnorderedIntMap v) where
  (<>) = union
  {-# INLINE (<>) #-}
#endif

instance Monoid (UnorderedIntMap v) where
  mempty = empty
  {-# INLINE mempty #-}
#if __GLASGOW_HASKELL__ >= 711
  mappend = (<>)
#else
  mappend = union
#endif
  {-# INLINE mappend #-}

instance Data v => Data (UnorderedIntMap v) where
    gfoldl f z m   = z fromList `f` toList m
    toConstr _     = fromListConstr
    gunfold k z c  = case constrIndex c of
        1 -> k (z fromList)
        _ -> error "gunfold"
    dataTypeOf _   = hashMapDataType

fromListConstr :: Constr
fromListConstr = mkConstr hashMapDataType "fromList" [] Prefix

hashMapDataType :: DataType
hashMapDataType = mkDataType "Data.UnorderedIntMap.UnorderedIntMap" [fromListConstr]

type Bitmap = Word
type Shift  = Int

#if MIN_VERSION_base(4,9,0)
instance Show1 UnorderedIntMap where
    liftShowsPrec spv slv d m =
        let sp = liftShowsPrec2 showsPrec showList spv slv
            sl = liftShowList2 showsPrec showList spv slv
        in showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)

instance Read1 UnorderedIntMap where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl
#endif

instance Read e => Read (UnorderedIntMap e) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance Show v => Show (UnorderedIntMap v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toList m)

instance Traversable UnorderedIntMap where
    traverse f = traverseWithKey (const f)

#if MIN_VERSION_base(4,9,0)
instance Eq1 UnorderedIntMap where
    liftEq = equal (==)
#endif

instance (Eq v) => Eq (UnorderedIntMap v) where
    (==) = equal (==) (==)

equal :: (Int -> Int -> Bool) -> (v -> v' -> Bool)
      -> UnorderedIntMap v -> UnorderedIntMap v' -> Bool
equal eqk eqv t1 t2 = go (toList' t1 []) (toList' t2 [])
  where
    -- If the two trees are the same, then their lists of 'Leaf's read from left to right should be the same

    go (Leaf l1 : tl1) (Leaf l2 : tl2)
      | leafEq l1 l2
      = go tl1 tl2
    go [] [] = True
    go _  _  = False

    leafEq (L k v) (L k' v') = eqk k k' && eqv v v'

#if MIN_VERSION_base(4,9,0)
instance Ord1 UnorderedIntMap where
    liftCompare = cmp compare
#endif

-- | The order is total.
instance Ord v => Ord (UnorderedIntMap v) where
    compare = cmp compare compare

cmp :: (Int -> Int -> Ordering) -> (v -> v' -> Ordering)
    -> UnorderedIntMap v -> UnorderedIntMap v' -> Ordering
cmp cmpk cmpv t1 t2 = go (toList' t1 []) (toList' t2 [])
  where
    go (Leaf l1 : tl1) (Leaf l2 : tl2)
      = leafCompare l1 l2 `mappend`
        go tl1 tl2
    go [] [] = EQ
    go [] _  = LT
    go _  [] = GT
    go _ _ = error "cmp: Should never happend, toList' includes non Leaf"

    leafCompare (L k v) (L k' v') = cmpk k k' `mappend` cmpv v v'

-- Same as 'equal' but doesn't compare the values.
equalKeys :: (Int -> Int -> Bool) -> UnorderedIntMap v -> UnorderedIntMap v' -> Bool
equalKeys eq t1 t2 = go (toList' t1 []) (toList' t2 [])
  where
    go (Leaf l1 : tl1) (Leaf l2 : tl2)
      | leafEq l1 l2
      = go tl1 tl2
    go [] [] = True
    go _  _  = False

    leafEq (L k _) (L k' _) = eq k k'

  -- Helper to get 'Leaf's as a list.
toList' :: UnorderedIntMap v -> [UnorderedIntMap v] -> [UnorderedIntMap v]
toList' (BitmapIndexed _ ary) a = A.foldr toList' a ary
toList' (Full ary)            a = A.foldr toList' a ary
toList' l@(Leaf _)          a = l : a
toList' Empty                 a = a

-- Helper function to detect 'Leaf's
isLeaf :: UnorderedIntMap v -> Bool
isLeaf (Leaf _)      = True
isLeaf _               = False

------------------------------------------------------------------------
-- * Construction

-- | /O(1)/ Construct an empty map.
empty :: UnorderedIntMap v
empty = Empty

-- | /O(1)/ Construct a map with a single element.
singleton :: Int -> v -> UnorderedIntMap v
singleton k v = Leaf (L k v)

------------------------------------------------------------------------
-- * Basic interface

-- | /O(1)/ Return 'True' if this map is empty, 'False' otherwise.
null :: UnorderedIntMap v -> Bool
null Empty = True
null _   = False

-- | /O(n)/ Return the number of key-value mappings in this map.
size :: UnorderedIntMap v -> Int
size t = go t 0
  where
    go Empty                !n = n
    go (Leaf _)            n = n + 1
    go (BitmapIndexed _ ary) n = A.foldl' (flip go) n ary
    go (Full ary)            n = A.foldl' (flip go) n ary

-- | /O(log n)/ Return 'True' if the specified key is present in the
-- map, 'False' otherwise.
member :: Int -> UnorderedIntMap a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True
{-# INLINABLE member #-}

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: Int -> UnorderedIntMap v -> Maybe v
lookup k0 m0 = go k0 0 m0
  where
    go !_ !_ Empty = Nothing
    go k _ (Leaf (L kx x))
        | k == kx = Just x  -- TODO: Split test in two
        | otherwise          = Nothing
    go k s (BitmapIndexed b v)
        | b .&. m == 0 = Nothing
        | otherwise    = go k (s+bitsPerSubkey) (A.index v (sparseIndex b m))
      where m = mask k s
    go k s (Full v) = go k (s+bitsPerSubkey) (A.index v (index k s))
{-# INLINABLE lookup #-}

-- | /O(log n)/ Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
lookupDefault :: v          -- ^ Default value to return.
              -> Int -> UnorderedIntMap v -> v
lookupDefault def k t = case lookup k t of
    Just v -> v
    _      -> def
{-# INLINABLE lookupDefault #-}

-- | /O(log n)/ Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: UnorderedIntMap v -> Int -> v
(!) m k = case lookup k m of
    Just v  -> v
    Nothing -> error "Data.UnorderedIntMap.(!): key not found"
{-# INLINABLE (!) #-}

infixl 9 !

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> A.Array (UnorderedIntMap v) -> UnorderedIntMap v
bitmapIndexedOrFull b ary
    | b == fullNodeMask = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: Int -> v -> UnorderedIntMap v -> UnorderedIntMap v
insert k0 v0 m0 = go k0 v0 0 m0
  where
    go !k x !_ Empty = Leaf (L k x)
    go k x s t@(Leaf (L ky y))
        | ky == k =
            if x `ptrEq` y
            then t
            else Leaf (L k x)
        | otherwise = runST (two s k x ky y)
    go k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 =
            let !ary' = A.insert ary i $! Leaf (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st  = A.index ary i
                !st' = go k x (s+bitsPerSubkey) st
            in if st' `ptrEq` st
               then t
               else BitmapIndexed b (A.update ary i st')
      where m = mask k s
            i = sparseIndex b m
    go k x s t@(Full ary) =
        let !st  = A.index ary i
            !st' = go k x (s+bitsPerSubkey) st
        in if st' `ptrEq` st
            then t
            else Full (update16 ary i st')
      where i = index k s
{-# INLINABLE insert #-}

-- | In-place update version of insert
unsafeInsert :: Int -> v -> UnorderedIntMap v -> UnorderedIntMap v
unsafeInsert k0 v0 m0 = runST (go k0 v0 0 m0)
  where
    go !k x !_ Empty = return $! Leaf (L k x)
    go k x s t@(Leaf (L ky y))
        | ky == k =
            if x `ptrEq` y
            then return t
            else return $! Leaf (L k x)
        | otherwise = two s k x ky y
    go k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 = do
            ary' <- A.insertM ary i $! Leaf (L k x)
            return $! bitmapIndexedOrFull (b .|. m) ary'
        | otherwise = do
            st <- A.indexM ary i
            st' <- go k x (s+bitsPerSubkey) st
            A.unsafeUpdateM ary i st'
            return t
      where m = mask k s
            i = sparseIndex b m
    go k x s t@(Full ary) = do
        st <- A.indexM ary i
        st' <- go k x (s+bitsPerSubkey) st
        A.unsafeUpdateM ary i st'
        return t
      where i = index k s
{-# INLINABLE unsafeInsert #-}

-- | Create a map from two key-value pairs which hashes don't collide.
two :: Shift -> Int -> v -> Int -> v -> ST s (UnorderedIntMap v)
two = go
  where
    go s k1 v1 k2 v2
        | bp1 == bp2 = do
            st <- go (s+bitsPerSubkey) k1 v1 k2 v2
            ary <- A.singletonM st
            return $! BitmapIndexed bp1 ary
        | otherwise  = do
            mary <- A.new 2 $ Leaf (L k1 v1)
            A.write mary idx2 $ Leaf (L k2 v2)
            ary <- A.unsafeFreeze mary
            return $! BitmapIndexed (bp1 .|. bp2) ary
      where
        bp1  = mask k1 s
        bp2  = mask k2 s
        idx2 | index k1 s < index k2 s = 1
             | otherwise               = 0
{-# INLINE two #-}

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (v -> v -> v) -> Int -> v -> UnorderedIntMap v
            -> UnorderedIntMap v
insertWith f k0 v0 m0 = go k0 v0 0 m0
  where
    go !k x !_ Empty = Leaf (L k x)
    go k x s (Leaf (L ky y))
        | ky == k = Leaf (L k (f x y))
        | otherwise = runST (two s k x ky y)
    go k x s (BitmapIndexed b ary)
        | b .&. m == 0 =
            let ary' = A.insert ary i $! Leaf (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let st   = A.index ary i
                st'  = go k x (s+bitsPerSubkey) st
                ary' = A.update ary i $! st'
            in BitmapIndexed b ary'
      where m = mask k s
            i = sparseIndex b m
    go k x s (Full ary) =
        let st   = A.index ary i
            st'  = go k x (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
      where i = index k s
{-# INLINABLE insertWith #-}

-- | In-place update version of insertWith
unsafeInsertWith :: forall v.
                 (v -> v -> v) -> Int -> v -> UnorderedIntMap v
                 -> UnorderedIntMap v
unsafeInsertWith f k0 v0 m0 = runST (go k0 v0 0 m0)
  where
    go :: Int -> v -> Shift -> UnorderedIntMap v -> ST s (UnorderedIntMap v)
    go !k x !_ Empty = return $! Leaf (L k x)
    go k x s (Leaf (L ky y))
        | ky == k = return $! Leaf (L k (f x y))
        | otherwise = two s k x ky y
    go k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 = do
            ary' <- A.insertM ary i $! Leaf (L k x)
            return $! bitmapIndexedOrFull (b .|. m) ary'
        | otherwise = do
            st <- A.indexM ary i
            st' <- go k x (s+bitsPerSubkey) st
            A.unsafeUpdateM ary i st'
            return t
      where m = mask k s
            i = sparseIndex b m
    go k x s t@(Full ary) = do
        st <- A.indexM ary i
        st' <- go k x (s+bitsPerSubkey) st
        A.unsafeUpdateM ary i st'
        return t
      where i = index k s
{-# INLINABLE unsafeInsertWith #-}

-- | /O(log n)/ Remove the mapping for the specified key from this map
-- if present.
delete :: Int -> UnorderedIntMap v -> UnorderedIntMap v
delete k0 m0 = go k0 0 m0
  where
    go !_ !_ Empty = Empty
    go k _ t@(Leaf (L ky _))
        | ky == k = Empty
        | otherwise          = t
    go k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise =
            let !st = A.index ary i
                !st' = go k (s+bitsPerSubkey) st
            in if st' `ptrEq` st
                then t
                else case st' of
                Empty | A.length ary == 1 -> Empty
                      | A.length ary == 2 ->
                          case (i, A.index ary 0, A.index ary 1) of
                          (0, _, l) | isLeaf l -> l
                          (1, l, _) | isLeaf l -> l
                          _                               -> bIndexed
                      | otherwise -> bIndexed
                    where
                      bIndexed = BitmapIndexed (b .&. complement m) (A.delete ary i)
                l | isLeaf l && A.length ary == 1 -> l
                _ -> BitmapIndexed b (A.update ary i st')
      where m = mask k s
            i = sparseIndex b m
    go k s t@(Full ary) =
        let !st   = A.index ary i
            !st' = go k (s+bitsPerSubkey) st
        in if st' `ptrEq` st
            then t
            else case st' of
            Empty ->
                let ary' = A.delete ary i
                    bm   = fullNodeMask .&. complement (1 `unsafeShiftL` i)
                in BitmapIndexed bm ary'
            _ -> Full (A.update ary i st')
      where i = index k s
{-# INLINABLE delete #-}

-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (v -> v) -> Int -> UnorderedIntMap v -> UnorderedIntMap v
adjust f k0 m0 = go k0 0 m0
  where
    go !_ !_ Empty = Empty
    go k _ t@(Leaf (L ky y))
        | ky == k = Leaf (L k (f y))
        | otherwise          = t
    go k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise = let st   = A.index ary i
                          st'  = go k (s+bitsPerSubkey) st
                          ary' = A.update ary i $! st'
                      in BitmapIndexed b ary'
      where m = mask k s
            i = sparseIndex b m
    go k s (Full ary) =
        let i    = index k s
            st   = A.index ary i
            st'  = go k (s+bitsPerSubkey) st
            ary' = update16 ary i $! st'
        in Full ary'
{-# INLINABLE adjust #-}

-- | /O(log n)/  The expression (@'update' f k map@) updates the value @x@ at @k@,
-- (if it is in the map). If (f k x) is @'Nothing', the element is deleted.
-- If it is (@'Just' y), the key k is bound to the new value y.
update :: (a -> Maybe a) -> Int -> UnorderedIntMap a -> UnorderedIntMap a
update f = alter (>>= f)
{-# INLINABLE update #-}


-- | /O(log n)/  The expression (@'alter' f k map@) alters the value @x@ at @k@, or
-- absence thereof. @alter@ can be used to insert, delete, or update a value in a
-- map. In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe v -> Maybe v) -> Int -> UnorderedIntMap v -> UnorderedIntMap v
alter f k m =
  case f (lookup k m) of
    Nothing -> delete k m
    Just v  -> insert k v m
{-# INLINABLE alter #-}

------------------------------------------------------------------------
-- * Combine

-- | /O(n+m)/ The union of two maps. If a key occurs in both maps, the
-- mapping from the first will be the mapping in the result.
union :: UnorderedIntMap v -> UnorderedIntMap v -> UnorderedIntMap v
union = unionWith const
{-# INLINABLE union #-}

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the
-- result.
unionWith :: (v -> v -> v) -> UnorderedIntMap v -> UnorderedIntMap v
          -> UnorderedIntMap v
unionWith f = unionWithKey (const f)
{-# INLINE unionWith #-}

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the
-- result.
unionWithKey :: (Int -> v -> v -> v) -> UnorderedIntMap v -> UnorderedIntMap v
          -> UnorderedIntMap v
unionWithKey f = go 0
  where
    -- empty vs. anything
    go !_ t1 Empty = t1
    go _ Empty t2 = t2
    -- leaf vs. leaf
    go s t1@(Leaf (L k1 v1)) t2@(Leaf (L k2 v2))
        | k1 == k2 = Leaf (L k1 (f k1 v1 v2))
        | otherwise = goDifferentHash s k1 k2 t1 t2
    -- branch vs. branch
    go s (BitmapIndexed b1 ary1) (BitmapIndexed b2 ary2) =
        let b'   = b1 .|. b2
            ary' = unionArrayBy (go (s+bitsPerSubkey)) b1 b2 ary1 ary2
        in bitmapIndexedOrFull b' ary'
    go s (BitmapIndexed b1 ary1) (Full ary2) =
        let ary' = unionArrayBy (go (s+bitsPerSubkey)) b1 fullNodeMask ary1 ary2
        in Full ary'
    go s (Full ary1) (BitmapIndexed b2 ary2) =
        let ary' = unionArrayBy (go (s+bitsPerSubkey)) fullNodeMask b2 ary1 ary2
        in Full ary'
    go s (Full ary1) (Full ary2) =
        let ary' = unionArrayBy (go (s+bitsPerSubkey)) fullNodeMask fullNodeMask
                   ary1 ary2
        in Full ary'
    -- leaf vs. branch
    go s (BitmapIndexed b1 ary1) t2
        | b1 .&. m2 == 0 = let ary' = A.insert ary1 i t2
                               b'   = b1 .|. m2
                           in bitmapIndexedOrFull b' ary'
        | otherwise      = let ary' = A.updateWith' ary1 i $ \st1 ->
                                   go (s+bitsPerSubkey) st1 t2
                           in BitmapIndexed b1 ary'
        where
          h2 = leafHashCode t2
          m2 = mask h2 s
          i = sparseIndex b1 m2
    go s t1 (BitmapIndexed b2 ary2)
        | b2 .&. m1 == 0 = let ary' = A.insert ary2 i $! t1
                               b'   = b2 .|. m1
                           in bitmapIndexedOrFull b' ary'
        | otherwise      = let ary' = A.updateWith' ary2 i $ \st2 ->
                                   go (s+bitsPerSubkey) t1 st2
                           in BitmapIndexed b2 ary'
      where
        h1 = leafHashCode t1
        m1 = mask h1 s
        i = sparseIndex b2 m1
    go s (Full ary1) t2 =
        let h2   = leafHashCode t2
            i    = index h2 s
            ary' = update16With' ary1 i $ \st1 -> go (s+bitsPerSubkey) st1 t2
        in Full ary'
    go s t1 (Full ary2) =
        let h1   = leafHashCode t1
            i    = index h1 s
            ary' = update16With' ary2 i $ \st2 -> go (s+bitsPerSubkey) t1 st2
        in Full ary'

    leafHashCode (Leaf (L k _)) = k
    leafHashCode _ = error "leafHashCode"

    goDifferentHash s h1 h2 t1 t2
        | m1 == m2  = BitmapIndexed m1 (A.singleton $! go (s+bitsPerSubkey) t1 t2)
        | m1 <  m2  = BitmapIndexed (m1 .|. m2) (A.pair t1 t2)
        | otherwise = BitmapIndexed (m1 .|. m2) (A.pair t2 t1)
      where
        m1 = mask h1 s
        m2 = mask h2 s
{-# INLINE unionWithKey #-}

-- | Strict in the result of @f@.
unionArrayBy :: (a -> a -> a) -> Bitmap -> Bitmap -> A.Array a -> A.Array a
             -> A.Array a
unionArrayBy f b1 b2 ary1 ary2 = A.run $ do
    let b' = b1 .|. b2
    mary <- A.new_ (popCount b')
    -- iterate over nonzero bits of b1 .|. b2
    -- it would be nice if we could shift m by more than 1 each time
    let ba = b1 .&. b2
        go !i !i1 !i2 !m
            | m > b'        = return ()
            | b' .&. m == 0 = go i i1 i2 (m `unsafeShiftL` 1)
            | ba .&. m /= 0 = do
                A.write mary i $! f (A.index ary1 i1) (A.index ary2 i2)
                go (i+1) (i1+1) (i2+1) (m `unsafeShiftL` 1)
            | b1 .&. m /= 0 = do
                A.write mary i =<< A.indexM ary1 i1
                go (i+1) (i1+1) (i2  ) (m `unsafeShiftL` 1)
            | otherwise     = do
                A.write mary i =<< A.indexM ary2 i2
                go (i+1) (i1  ) (i2+1) (m `unsafeShiftL` 1)
    go 0 0 0 (b' .&. negate b') -- XXX: b' must be non-zero
    return mary
    -- TODO: For the case where b1 .&. b2 == b1, i.e. when one is a
    -- subset of the other, we could use a slightly simpler algorithm,
    -- where we copy one array, and then update.
{-# INLINE unionArrayBy #-}

-- TODO: Figure out the time complexity of 'unions'.

-- | Construct a set containing all elements from a list of sets.
unions :: [UnorderedIntMap v] -> UnorderedIntMap v
unions = L.foldl' union empty
{-# INLINE unions #-}

------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
mapWithKey :: (Int -> v1 -> v2) -> UnorderedIntMap v1 -> UnorderedIntMap v2
mapWithKey f = go
  where
    go Empty = Empty
    go (Leaf (L k v)) = Leaf $ L k (f k v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map' go ary
    go (Full ary) = Full $ A.map' go ary
{-# INLINE mapWithKey #-}

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> UnorderedIntMap v1 -> UnorderedIntMap v2
map f = mapWithKey (const f)
{-# INLINE map #-}

-- TODO: We should be able to use mutation to create the new
-- 'UnorderedIntMap'.

-- | /O(n)/ Transform this map by accumulating an Applicative result
-- from every value.
traverseWithKey :: Applicative f => (Int -> v1 -> f v2) -> UnorderedIntMap v1
                -> f (UnorderedIntMap v2)
traverseWithKey f = go
  where
    go Empty                 = pure Empty
    go (Leaf (L k v))      = Leaf . L k <$> f k v
    go (BitmapIndexed b ary) = BitmapIndexed b <$> A.traverse go ary
    go (Full ary)            = Full <$> A.traverse go ary
{-# INLINE traverseWithKey #-}

------------------------------------------------------------------------
-- * Difference and intersection

-- | /O(n*log m)/ Difference of two maps. Return elements of the first map
-- not existing in the second.
difference :: UnorderedIntMap v -> UnorderedIntMap w -> UnorderedIntMap v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> insert k v m
                 _       -> m
{-# INLINABLE difference #-}

-- | /O(n*log m)/ Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWith :: (v -> w -> Maybe v) -> UnorderedIntMap v -> UnorderedIntMap w -> UnorderedIntMap v
differenceWith f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> insert k v m
                 Just w  -> maybe m (\y -> insert k y m) (f v w)
{-# INLINABLE differenceWith #-}

-- | /O(n*log m)/ Intersection of two maps. Return elements of the first
-- map for keys existing in the second.
intersection :: UnorderedIntMap v -> UnorderedIntMap w -> UnorderedIntMap v
intersection a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just _ -> insert k v m
                 _      -> m
{-# INLINABLE intersection #-}

-- | /O(n+m)/ Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWith :: (v1 -> v2 -> v3) -> UnorderedIntMap v1
                 -> UnorderedIntMap v2 -> UnorderedIntMap v3
intersectionWith f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just w -> insert k (f v w) m
                 _      -> m
{-# INLINABLE intersectionWith #-}

-- | /O(n+m)/ Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWithKey :: (Int -> v1 -> v2 -> v3)
                    -> UnorderedIntMap v1 -> UnorderedIntMap v2 -> UnorderedIntMap v3
intersectionWithKey f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Just w -> insert k (f k v w) m
                 _      -> m
{-# INLINABLE intersectionWithKey #-}

------------------------------------------------------------------------
-- * Folds

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> UnorderedIntMap v -> a
foldl' f = foldlWithKey' (\ z _ v -> f z v)
{-# INLINE foldl' #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldlWithKey' :: (a -> Int -> v -> a) -> a -> UnorderedIntMap v -> a
foldlWithKey' f = go
  where
    go !z Empty                = z
    go z (Leaf (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl' go z ary
    go z (Full ary)            = A.foldl' go z ary
{-# INLINE foldlWithKey' #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> UnorderedIntMap v -> a
foldr f = foldrWithKey (const f)
{-# INLINE foldr #-}

-- | /O(n)/ Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (Int -> v -> a -> a) -> a -> UnorderedIntMap v -> a
foldrWithKey f = go
  where
    go z Empty                 = z
    go z (Leaf (L k v))      = f k v z
    go z (BitmapIndexed _ ary) = A.foldr (flip go) z ary
    go z (Full ary)            = A.foldr (flip go) z ary
{-# INLINE foldrWithKey #-}

------------------------------------------------------------------------
-- * Filter

-- | Create a new array of the @n@ first elements of @mary@.
trim :: A.MArray s a -> Int -> ST s (A.Array a)
trim mary n = do
    mary2 <- A.new_ n
    A.copyM mary 0 mary2 0 n
    A.unsafeFreeze mary2
{-# INLINE trim #-}

-- | /O(n)/ Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybeWithKey :: (Int -> v1 -> Maybe v2) -> UnorderedIntMap v1 -> UnorderedIntMap v2
mapMaybeWithKey f = filterMapAux onLeaf
  where onLeaf (Leaf (L k v)) | Just v' <- f k v = Just (Leaf (L k v'))
        onLeaf _ = Nothing
{-# INLINE mapMaybeWithKey #-}

-- | /O(n)/ Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybe :: (v1 -> Maybe v2) -> UnorderedIntMap v1 -> UnorderedIntMap v2
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | /O(n)/ Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: forall v. (Int -> v -> Bool) -> UnorderedIntMap v -> UnorderedIntMap v
filterWithKey pred = filterMapAux onLeaf
  where onLeaf t@(Leaf (L k v)) | pred k v = Just t
        onLeaf _ = Nothing
{-# INLINE filterWithKey #-}


-- | Common implementation for 'filterWithKey' and 'mapMaybeWithKey',
--   allowing the former to former to reuse terms.
filterMapAux :: forall v1 v2
              . (UnorderedIntMap v1 -> Maybe (UnorderedIntMap v2))
             -> UnorderedIntMap v1
             -> UnorderedIntMap v2
filterMapAux onLeaf = go
  where
    go Empty = Empty
    go t@Leaf{}
        | Just t' <- onLeaf t = t'
        | otherwise = Empty
    go (BitmapIndexed b ary) = filterA ary b
    go (Full ary) = filterA ary fullNodeMask

    filterA ary0 b0 =
        let !n = A.length ary0
        in runST $ do
            mary <- A.new_ n
            step ary0 mary b0 0 0 1 n
      where
        step :: A.Array (UnorderedIntMap v1) -> A.MArray s (UnorderedIntMap v2)
             -> Bitmap -> Int -> Int -> Bitmap -> Int
             -> ST s (UnorderedIntMap v2)
        step !ary !mary !b i !j !bi n
            | i >= n = case j of
                0 -> return Empty
                1 -> do
                    ch <- A.read mary 0
                    case ch of
                      t | isLeaf t -> return t
                      _                       -> BitmapIndexed b <$> trim mary 1
                _ -> do
                    ary2 <- trim mary j
                    return $! if j == maxChildren
                              then Full ary2
                              else BitmapIndexed b ary2
            | bi .&. b == 0 = step ary mary b i j (bi `unsafeShiftL` 1) n
            | otherwise = case go (A.index ary i) of
                Empty -> step ary mary (b .&. complement bi) (i+1) j
                         (bi `unsafeShiftL` 1) n
                t     -> do A.write mary j t
                            step ary mary b (i+1) (j+1) (bi `unsafeShiftL` 1) n
{-# INLINE filterMapAux #-}

-- | /O(n)/ Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (v -> Bool) -> UnorderedIntMap v -> UnorderedIntMap v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

------------------------------------------------------------------------
-- * Conversions

-- TODO: Improve fusion rules by modelled them after the Prelude ones
-- on lists.

-- | /O(n)/ Return a list of this map's keys.  The list is produced
-- lazily.
keys :: UnorderedIntMap v -> [Int]
keys = L.map fst . toList
{-# INLINE keys #-}

-- | /O(n)/ Return a list of this map's values.  The list is produced
-- lazily.
elems :: UnorderedIntMap v -> [v]
elems = L.map snd . toList
{-# INLINE elems #-}

------------------------------------------------------------------------
-- ** Lists

-- | /O(n)/ Return a list of this map's elements.  The list is
-- produced lazily. The order of its elements is unspecified.
toList :: UnorderedIntMap v -> [(Int, v)]
toList t = build (\ c z -> foldrWithKey (curry c) z t)
{-# INLINE toList #-}

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: [(Int, v)] -> UnorderedIntMap v
fromList = L.foldl' (\ m (k, v) -> unsafeInsert k v m) empty
{-# INLINABLE fromList #-}

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
fromListWith :: (v -> v -> v) -> [(Int, v)] -> UnorderedIntMap v
fromListWith f = L.foldl' (\ m (k, v) -> unsafeInsertWith f k v m) empty
{-# INLINE fromListWith #-}

------------------------------------------------------------------------
-- Array operations

-- | /O(n)/ Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
indexOf :: Int -> A.Array (Leaf v) -> Maybe Int
indexOf k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = Nothing
        | otherwise = case A.index ary i of
            (L kx _)
                | k == kx   -> Just i
                | otherwise -> go k ary (i+1) n
{-# INLINABLE indexOf #-}

updateOrConcatWith :: (v -> v -> v) -> A.Array (Leaf v) -> A.Array (Leaf v) -> A.Array (Leaf v)
updateOrConcatWith f = updateOrConcatWithKey (const f)
{-# INLINABLE updateOrConcatWith #-}

updateOrConcatWithKey :: (Int -> v -> v -> v) -> A.Array (Leaf v) -> A.Array (Leaf v) -> A.Array (Leaf v)
updateOrConcatWithKey f ary1 ary2 = A.run $ do
    -- first: look up the position of each element of ary2 in ary1
    let indices = A.map (\(L k _) -> indexOf k ary1) ary2
    -- that tells us how large the overlap is:
    -- count number of Nothing constructors
    let nOnly2 = A.foldl' (\n -> maybe (n+1) (const n)) 0 indices
    let n1 = A.length ary1
    let n2 = A.length ary2
    -- copy over all elements from ary1
    mary <- A.new_ (n1 + nOnly2)
    A.copy ary1 0 mary 0 n1
    -- append or update all elements from ary2
    let go !iEnd !i2
          | i2 >= n2 = return ()
          | otherwise = case A.index indices i2 of
               Just i1 -> do -- key occurs in both arrays, store combination in position i1
                             L k v1 <- A.indexM ary1 i1
                             L _ v2 <- A.indexM ary2 i2
                             A.write mary i1 (L k (f k v1 v2))
                             go iEnd (i2+1)
               Nothing -> do -- key is only in ary2, append to end
                             A.write mary iEnd =<< A.indexM ary2 i2
                             go (iEnd+1) (i2+1)
    go n1 0
    return mary
{-# INLINABLE updateOrConcatWithKey #-}

------------------------------------------------------------------------
-- Manually unrolled loops

-- | /O(n)/ Update the element at the given position in this array.
update16 :: A.Array e -> Int -> e -> A.Array e
update16 ary idx b = runST (update16M ary idx b)
{-# INLINE update16 #-}

-- | /O(n)/ Update the element at the given position in this array.
update16M :: A.Array e -> Int -> e -> ST s (A.Array e)
update16M ary idx b = do
    mary <- clone16 ary
    A.write mary idx b
    A.unsafeFreeze mary
{-# INLINE update16M #-}

-- | /O(n)/ Update the element at the given position in this array, by applying a function to it.
update16With' :: A.Array e -> Int -> (e -> e) -> A.Array e
update16With' ary idx f = update16 ary idx $! f (A.index ary idx)
{-# INLINE update16With' #-}

-- | Unsafely clone an array of 16 elements.  The length of the input
-- array is not checked.
clone16 :: A.Array e -> ST s (A.MArray s e)
clone16 ary =
    A.thaw ary 0 16

------------------------------------------------------------------------
-- Bit twiddling

bitsPerSubkey :: Int
bitsPerSubkey = 4

maxChildren :: Int
maxChildren = fromIntegral $ 1 `unsafeShiftL` bitsPerSubkey

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))

mask :: Int -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}

-- | Mask out the 'bitsPerSubkey' bits used for indexing at this level
-- of the tree.
index :: Int -> Shift -> Int
index w s = fromIntegral $ (unsafeShiftR (fromIntegral w) s) .&. subkeyMask
{-# INLINE index #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

------------------------------------------------------------------------
-- IsList instance
instance Exts.IsList (UnorderedIntMap v) where
    type Item (UnorderedIntMap v) = (Int, v)
    fromList = fromList
    toList   = toList