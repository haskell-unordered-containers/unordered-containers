{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UnboxedSums           #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.

module Data.HashMap.Internal
    (
      HashMap(..)
    , Leaf(..)

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , member
    , lookup
    , (!?)
    , findWithDefault
    , lookupDefault
    , (!)
    , insert
    , insertWith
    , unsafeInsert
    , delete
    , adjust
    , update
    , alter
    , alterF
    , isSubmapOf
    , isSubmapOfBy

      -- * Combine
      -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions

    -- ** Compose
    , compose

      -- * Transformations
    , map
    , mapWithKey
    , traverseWithKey
    , mapKeys

      -- * Difference and intersection
    , difference
    , differenceWith
    , intersection
    , intersectionWith
    , intersectionWithKey
    , intersectionWithKey#

      -- * Folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

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
    , fromListWithKey

      -- ** Internals used by the strict version
    , Hash
    , Bitmap
    , Shift
    , bitmapIndexedOrFull
    , collision
    , hash
    , mask
    , index
    , bitsPerSubkey
    , maxChildren
    , isLeafOrCollision
    , fullBitmap
    , subkeyMask
    , nextShift
    , sparseIndex
    , two
    , unionArrayBy
    , update32
    , update32M
    , update32With'
    , updateOrConcatWithKey
    , filterMapAux
    , equalKeys
    , equalKeys1
    , lookupRecordCollision
    , LookupRes(..)
    , lookupResToMaybe
    , insert'
    , delete'
    , lookup'
    , insertNewKey
    , insertKeyExists
    , deleteKeyExists
    , insertModifying
    , ptrEq
    , adjust#
    ) where

import Control.Applicative        (Const (..))
import Control.DeepSeq            (NFData (..), NFData1 (..), NFData2 (..))
import Control.Monad.ST           (ST, runST)
import Data.Bifoldable            (Bifoldable (..))
import Data.Bits                  (complement, countTrailingZeros, popCount,
                                   shiftL, unsafeShiftL, unsafeShiftR, (.&.),
                                   (.|.))
import Data.Coerce                (coerce)
import Data.Data                  (Constr, Data (..), DataType)
import Data.Functor.Classes       (Eq1 (..), Eq2 (..), Ord1 (..), Ord2 (..),
                                   Read1 (..), Show1 (..), Show2 (..))
import Data.Functor.Identity      (Identity (..))
import Data.Hashable              (Hashable)
import Data.Hashable.Lifted       (Hashable1, Hashable2)
import Data.HashMap.Internal.List (isPermutationBy, unorderedCompare)
import Data.Semigroup             (Semigroup (..), stimesIdempotentMonoid)
import GHC.Exts                   (Int (..), Int#, TYPE, (==#))
import GHC.Stack                  (HasCallStack)
import Prelude                    hiding (Foldable(..), filter, lookup, map,
                                   pred)
import Text.Read                  hiding (step)

import qualified Data.Data                   as Data
import qualified Data.Foldable               as Foldable
import qualified Data.Functor.Classes        as FC
import qualified Data.Hashable               as H
import qualified Data.Hashable.Lifted        as H
import qualified Data.HashMap.Internal.Array as A
import qualified Data.List                   as List
import qualified GHC.Exts                    as Exts
import qualified Language.Haskell.TH.Syntax  as TH

-- | Convenience function.  Compute a hash value for the given value.
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L !k v
  deriving (Eq)

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L k v) = rnf k `seq` rnf v

-- | @since 0.2.17.0
instance (TH.Lift k, TH.Lift v) => TH.Lift (Leaf k v) where
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (L k v) = [|| L k $! v ||]
#else
  lift (L k v) = [| L k $! v |]
#endif

-- | @since 0.2.14.0
instance NFData k => NFData1 (Leaf k) where
    liftRnf = liftRnf2 rnf

-- | @since 0.2.14.0
instance NFData2 Leaf where
    liftRnf2 rnf1 rnf2 (L k v) = rnf1 k `seq` rnf2 v

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Empty
    -- ^ Invariants:
    --
    -- * 'Empty' is not a valid sub-node. It can only appear at the root. (INV1)
    | BitmapIndexed !Bitmap !(A.Array (HashMap k v))
    -- ^ Invariants:
    --
    -- * Only the lower @maxChildren@ bits of the 'Bitmap' may be set. The
    --   remaining upper bits must be 0. (INV2)
    -- * The array of a 'BitmapIndexed' node stores at least 1 and at most
    --   @'maxChildren' - 1@ sub-nodes. (INV3)
    -- * The number of sub-nodes is equal to the number of 1-bits in its
    --   'Bitmap'. (INV4)
    -- * If a 'BitmapIndexed' node has only one sub-node, this sub-node must
    --   be a 'BitmapIndexed' or a 'Full' node. (INV5)
    | Leaf !Hash !(Leaf k v)
    -- ^ Invariants:
    --
    -- * The location of a 'Leaf' or 'Collision' node in the tree must be
    --   compatible with its 'Hash'. (INV6)
    --   (TODO: Document this properly (#425))
    -- * The 'Hash' of a 'Leaf' node must be the 'hash' of its key. (INV7)
    | Full !(A.Array (HashMap k v))
    -- ^ Invariants:
    --
    -- * The array of a 'Full' node stores exactly 'maxChildren' sub-nodes. (INV8)
    | Collision !Hash !(A.Array (Leaf k v))
    -- ^ Invariants:
    --
    -- * The location of a 'Leaf' or 'Collision' node in the tree must be
    --   compatible with its 'Hash'. (INV6)
    --   (TODO: Document this properly (#425))
    -- * The array of a 'Collision' node must contain at least two sub-nodes. (INV9)
    -- * The 'hash' of each key in a 'Collision' node must be the one stored in
    --   the node. (INV7)
    -- * No two keys stored in a 'Collision' can be equal according to their
    --   'Eq' instance. (INV10)

type role HashMap nominal representational

-- | @since 0.2.17.0
deriving instance (TH.Lift k, TH.Lift v) => TH.Lift (HashMap k v)

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf _ l)            = rnf l
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

-- | @since 0.2.14.0
instance NFData k => NFData1 (HashMap k) where
    liftRnf = liftRnf2 rnf

-- | @since 0.2.14.0
instance NFData2 HashMap where
    liftRnf2 _ _ Empty                       = ()
    liftRnf2 rnf1 rnf2 (BitmapIndexed _ ary) = liftRnf (liftRnf2 rnf1 rnf2) ary
    liftRnf2 rnf1 rnf2 (Leaf _ l)            = liftRnf2 rnf1 rnf2 l
    liftRnf2 rnf1 rnf2 (Full ary)            = liftRnf (liftRnf2 rnf1 rnf2) ary
    liftRnf2 rnf1 rnf2 (Collision _ ary)     = liftRnf (liftRnf2 rnf1 rnf2) ary

instance Functor (HashMap k) where
    fmap = map

instance Foldable.Foldable (HashMap k) where
    foldMap f = foldMapWithKey (\ _k v -> f v)
    {-# INLINE foldMap #-}
    foldr = foldr
    {-# INLINE foldr #-}
    foldl = foldl
    {-# INLINE foldl #-}
    foldr' = foldr'
    {-# INLINE foldr' #-}
    foldl' = foldl'
    {-# INLINE foldl' #-}
    null = null
    {-# INLINE null #-}
    length = size
    {-# INLINE length #-}

-- | @since 0.2.11
instance Bifoldable HashMap where
    bifoldMap f g = foldMapWithKey (\ k v -> f k `mappend` g v)
    {-# INLINE bifoldMap #-}
    bifoldr f g = foldrWithKey (\ k v acc -> k `f` (v `g` acc))
    {-# INLINE bifoldr #-}
    bifoldl f g = foldlWithKey (\ acc k v -> (acc `f` k) `g` v)
    {-# INLINE bifoldl #-}

-- | '<>' = 'union'
--
-- If a key occurs in both maps, the mapping from the first will be the mapping in the result.
--
-- ==== __Examples__
--
-- >>> fromList [(1,'a'),(2,'b')] <> fromList [(2,'c'),(3,'d')]
-- fromList [(1,'a'),(2,'b'),(3,'d')]
instance (Eq k, Hashable k) => Semigroup (HashMap k v) where
  (<>) = union
  {-# INLINE (<>) #-}
  stimes = stimesIdempotentMonoid
  {-# INLINE stimes #-}

-- | 'mempty' = 'empty'
--
-- 'mappend' = 'union'
--
-- If a key occurs in both maps, the mapping from the first will be the mapping in the result.
--
-- ==== __Examples__
--
-- >>> mappend (fromList [(1,'a'),(2,'b')]) (fromList [(2,'c'),(3,'d')])
-- fromList [(1,'a'),(2,'b'),(3,'d')]
instance (Eq k, Hashable k) => Monoid (HashMap k v) where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance (Data k, Data v, Eq k, Hashable k) => Data (HashMap k v) where
    gfoldl f z m   = z fromList `f` toList m
    toConstr _     = fromListConstr
    gunfold k z c  = case Data.constrIndex c of
        1 -> k (z fromList)
        _ -> error "gunfold"
    dataTypeOf _   = hashMapDataType
    dataCast1 f    = Data.gcast1 f
    dataCast2 f    = Data.gcast2 f

fromListConstr :: Constr
fromListConstr = Data.mkConstr hashMapDataType "fromList" [] Data.Prefix

hashMapDataType :: DataType
hashMapDataType = Data.mkDataType "Data.HashMap.Internal.HashMap" [fromListConstr]

-- | This type is used to store the hash of a key, as produced with 'hash'.
type Hash   = Word

-- | A bitmap as contained by a 'BitmapIndexed' node, or a 'fullBitmap'
-- corresponding to a 'Full' node.
--
-- Only the lower 'maxChildren' bits are used. The remaining bits must be zeros.
type Bitmap = Word

-- | 'Shift' values correspond to the level of the tree that we're currently
-- operating at. At the root level the 'Shift' is @0@. For the subsequent
-- levels the 'Shift' values are 'bitsPerSubkey', @2*'bitsPerSubkey'@ etc.
--
-- Valid values are non-negative and less than @bitSize (0 :: Word)@.
type Shift  = Int

instance Show2 HashMap where
    liftShowsPrec2 spk slk spv slv d m =
        FC.showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (HashMap k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq k, Hashable k, Read k) => Read1 (HashMap k) where
    liftReadsPrec rp rl = FC.readsData $
        FC.readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Eq k, Hashable k, Read k, Read e) => Read (HashMap k e) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fromList <$> readPrec

    readListPrec = readListPrecDefault

instance (Show k, Show v) => Show (HashMap k v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toList m)

instance Traversable (HashMap k) where
    traverse f = traverseWithKey (const f)
    {-# INLINABLE traverse #-}

instance Eq2 HashMap where
    liftEq2 = equal2

instance Eq k => Eq1 (HashMap k) where
    liftEq = equal1

-- | Note that, in the presence of hash collisions, equal @HashMap@s may
-- behave differently, i.e. extensionality may be violated:
--
-- >>> data D = A | B deriving (Eq, Show)
-- >>> instance Hashable D where hashWithSalt salt _d = salt
--
-- >>> x = fromList [(A,1), (B,2)]
-- >>> y = fromList [(B,2), (A,1)]
--
-- >>> x == y
-- True
-- >>> toList x
-- [(A,1),(B,2)]
-- >>> toList y
-- [(B,2),(A,1)]
--
-- In general, the lack of extensionality can be observed with any function
-- that depends on the key ordering, such as folds and traversals.
instance (Eq k, Eq v) => Eq (HashMap k v) where
    (==) = equal1 (==)

equal1 :: Eq k
       => (v -> v' -> Bool)
       -> HashMap k v -> HashMap k v' -> Bool
equal1 eq = go
  where
    go Empty Empty = True
    go (BitmapIndexed bm1 ary1) (BitmapIndexed bm2 ary2)
      = bm1 == bm2 && A.sameArray1 go ary1 ary2
    go (Leaf h1 l1) (Leaf h2 l2) = h1 == h2 && leafEq l1 l2
    go (Full ary1) (Full ary2) = A.sameArray1 go ary1 ary2
    go (Collision h1 ary1) (Collision h2 ary2)
      = h1 == h2 && isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
    go _ _ = False

    leafEq (L k1 v1) (L k2 v2) = k1 == k2 && eq v1 v2

equal2 :: (k -> k' -> Bool) -> (v -> v' -> Bool)
      -> HashMap k v -> HashMap k' v' -> Bool
equal2 eqk eqv t1 t2 = go (leavesAndCollisions t1 []) (leavesAndCollisions t2 [])
  where
    -- If the two trees are the same, then their lists of 'Leaf's and
    -- 'Collision's read from left to right should be the same (modulo the
    -- order of elements in 'Collision').

    go (Leaf k1 l1 : tl1) (Leaf k2 l2 : tl2)
      | k1 == k2 &&
        leafEq l1 l2
      = go tl1 tl2
    go (Collision h1 ary1 : tl1) (Collision h2 ary2 : tl2)
      | h1 == h2 &&
        A.length ary1 == A.length ary2 &&
        isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
      = go tl1 tl2
    go [] [] = True
    go _  _  = False

    leafEq (L k v) (L k' v') = eqk k k' && eqv v v'

instance Ord2 HashMap where
    liftCompare2 = cmp

instance Ord k => Ord1 (HashMap k) where
    liftCompare = cmp compare

-- | The ordering is total and consistent with the `Eq` instance. However,
-- nothing else about the ordering is specified, and it may change from
-- version to version of either this package or of hashable.
instance (Ord k, Ord v) => Ord (HashMap k v) where
    compare = cmp compare compare

cmp :: (k -> k' -> Ordering) -> (v -> v' -> Ordering)
    -> HashMap k v -> HashMap k' v' -> Ordering
cmp cmpk cmpv t1 t2 = go (leavesAndCollisions t1 []) (leavesAndCollisions t2 [])
  where
    go (Leaf k1 l1 : tl1) (Leaf k2 l2 : tl2)
      = compare k1 k2 `mappend`
        leafCompare l1 l2 `mappend`
        go tl1 tl2
    go (Collision h1 ary1 : tl1) (Collision h2 ary2 : tl2)
      = compare h1 h2 `mappend`
        compare (A.length ary1) (A.length ary2) `mappend`
        unorderedCompare leafCompare (A.toList ary1) (A.toList ary2) `mappend`
        go tl1 tl2
    go (Leaf _ _ : _) (Collision _ _ : _) = LT
    go (Collision _ _ : _) (Leaf _ _ : _) = GT
    go [] [] = EQ
    go [] _  = LT
    go _  [] = GT
    go _ _ = error "cmp: Should never happen, leavesAndCollisions includes non Leaf / Collision"

    leafCompare (L k v) (L k' v') = cmpk k k' `mappend` cmpv v v'

-- Same as 'equal2' but doesn't compare the values.
equalKeys1 :: (k -> k' -> Bool) -> HashMap k v -> HashMap k' v' -> Bool
equalKeys1 eq t1 t2 = go (leavesAndCollisions t1 []) (leavesAndCollisions t2 [])
  where
    go (Leaf k1 l1 : tl1) (Leaf k2 l2 : tl2)
      | k1 == k2 && leafEq l1 l2
      = go tl1 tl2
    go (Collision h1 ary1 : tl1) (Collision h2 ary2 : tl2)
      | h1 == h2 && A.length ary1 == A.length ary2 &&
        isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
      = go tl1 tl2
    go [] [] = True
    go _  _  = False

    leafEq (L k _) (L k' _) = eq k k'

-- Same as 'equal1' but doesn't compare the values.
equalKeys :: Eq k => HashMap k v -> HashMap k v' -> Bool
equalKeys = go
  where
    go :: Eq k => HashMap k v -> HashMap k v' -> Bool
    go Empty Empty = True
    go (BitmapIndexed bm1 ary1) (BitmapIndexed bm2 ary2)
      = bm1 == bm2 && A.sameArray1 go ary1 ary2
    go (Leaf h1 l1) (Leaf h2 l2) = h1 == h2 && leafEq l1 l2
    go (Full ary1) (Full ary2) = A.sameArray1 go ary1 ary2
    go (Collision h1 ary1) (Collision h2 ary2)
      = h1 == h2 && isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
    go _ _ = False

    leafEq (L k1 _) (L k2 _) = k1 == k2

instance Hashable2 HashMap where
    liftHashWithSalt2 hk hv salt hm = go salt (leavesAndCollisions hm [])
      where
        -- go :: Int -> [HashMap k v] -> Int
        go s [] = s
        go s (Leaf _ l : tl)
          = s `hashLeafWithSalt` l `go` tl
        -- For collisions we hashmix hash value
        -- and then array of values' hashes sorted
        go s (Collision h a : tl)
          = (s `H.hashWithSalt` h) `hashCollisionWithSalt` a `go` tl
        go s (_ : tl) = s `go` tl

        -- hashLeafWithSalt :: Int -> Leaf k v -> Int
        hashLeafWithSalt s (L k v) = (s `hk` k) `hv` v

        -- hashCollisionWithSalt :: Int -> A.Array (Leaf k v) -> Int
        hashCollisionWithSalt s
          = List.foldl' H.hashWithSalt s . arrayHashesSorted s

        -- arrayHashesSorted :: Int -> A.Array (Leaf k v) -> [Int]
        arrayHashesSorted s = List.sort . List.map (hashLeafWithSalt s) . A.toList

instance (Hashable k) => Hashable1 (HashMap k) where
    liftHashWithSalt = H.liftHashWithSalt2 H.hashWithSalt

instance (Hashable k, Hashable v) => Hashable (HashMap k v) where
    hashWithSalt salt hm = go salt hm
      where
        go :: Int -> HashMap k v -> Int
        go s Empty = s
        go s (BitmapIndexed _ a) = A.foldl' go s a
        go s (Leaf h (L _ v))
          = s `H.hashWithSalt` h `H.hashWithSalt` v
        -- For collisions we hashmix hash value
        -- and then array of values' hashes sorted
        go s (Full a) = A.foldl' go s a
        go s (Collision h a)
          = (s `H.hashWithSalt` h) `hashCollisionWithSalt` a

        hashLeafWithSalt :: Int -> Leaf k v -> Int
        hashLeafWithSalt s (L k v) = s `H.hashWithSalt` k `H.hashWithSalt` v

        hashCollisionWithSalt :: Int -> A.Array (Leaf k v) -> Int
        hashCollisionWithSalt s
          = List.foldl' H.hashWithSalt s . arrayHashesSorted s

        arrayHashesSorted :: Int -> A.Array (Leaf k v) -> [Int]
        arrayHashesSorted s = List.sort . List.map (hashLeafWithSalt s) . A.toList

-- | Helper to get 'Leaf's and 'Collision's as a list.
leavesAndCollisions :: HashMap k v -> [HashMap k v] -> [HashMap k v]
leavesAndCollisions (BitmapIndexed _ ary) a = A.foldr leavesAndCollisions a ary
leavesAndCollisions (Full ary)            a = A.foldr leavesAndCollisions a ary
leavesAndCollisions l@(Leaf _ _)          a = l : a
leavesAndCollisions c@(Collision _ _)     a = c : a
leavesAndCollisions Empty                 a = a

-- | Helper function to detect 'Leaf's and 'Collision's.
isLeafOrCollision :: HashMap k v -> Bool
isLeafOrCollision (Leaf _ _)      = True
isLeafOrCollision (Collision _ _) = True
isLeafOrCollision _               = False

------------------------------------------------------------------------
-- * Construction

-- | \(O(1)\) Construct an empty map.
empty :: HashMap k v
empty = Empty

-- | \(O(1)\) Construct a map with a single element.
singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k v = Leaf (hash k) (L k v)

------------------------------------------------------------------------
-- * Basic interface

-- | \(O(1)\) Return 'True' if this map is empty, 'False' otherwise.
null :: HashMap k v -> Bool
null Empty = True
null _   = False

-- | \(O(n)\) Return the number of key-value mappings in this map.
size :: HashMap k v -> Int
size t = go t 0
  where
    go Empty                !n = n
    go (Leaf _ _)            n = n + 1
    go (BitmapIndexed _ ary) n = A.foldl' (flip go) n ary
    go (Full ary)            n = A.foldl' (flip go) n ary
    go (Collision _ ary)     n = n + A.length ary

-- | \(O(\log n)\) Return 'True' if the specified key is present in the
-- map, 'False' otherwise.
member :: (Eq k, Hashable k) => k -> HashMap k a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True
{-# INLINABLE member #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
-- GHC does not yet perform a worker-wrapper transformation on
-- unboxed sums automatically. That seems likely to happen at some
-- point (possibly as early as GHC 8.6) but for now we do it manually.
lookup k m = case lookup# k m of
  (# (# #) | #) -> Nothing
  (# | a #) -> Just a
{-# INLINE lookup #-}

lookup# :: (Eq k, Hashable k) => k -> HashMap k v -> (# (# #) | v #)
lookup# k m = lookupCont (\_ -> (# (# #) | #)) (\v _i -> (# | v #)) (hash k) k 0 m
{-# INLINABLE lookup# #-}

-- | lookup' is a version of lookup that takes the hash separately.
-- It is used to implement alterF.
lookup' :: Eq k => Hash -> k -> HashMap k v -> Maybe v
-- GHC does not yet perform a worker-wrapper transformation on
-- unboxed sums automatically. That seems likely to happen at some
-- point (possibly as early as GHC 8.6) but for now we do it manually.
-- lookup' would probably prefer to be implemented in terms of its own
-- lookup'#, but it's not important enough and we don't want too much
-- code.
lookup' h k m = case lookupRecordCollision# h k m of
  (# (# #) | #) -> Nothing
  (# | (# a, _i #) #) -> Just a
{-# INLINE lookup' #-}

-- The result of a lookup, keeping track of if a hash collision occurred.
-- If a collision did not occur then it will have the Int value (-1).
data LookupRes a = Absent | Present a !Int

lookupResToMaybe :: LookupRes a -> Maybe a
lookupResToMaybe Absent        = Nothing
lookupResToMaybe (Present x _) = Just x
{-# INLINE lookupResToMaybe #-}

-- Internal helper for lookup. This version takes the precomputed hash so
-- that functions that make multiple calls to lookup and related functions
-- (insert, delete) only need to calculate the hash once.
--
-- It is used by 'alterF' so that hash computation and key comparison only needs
-- to be performed once. With this information you can use the more optimized
-- versions of insert ('insertNewKey', 'insertKeyExists') and delete
-- ('deleteKeyExists')
--
-- Outcomes:
--   Key not in map           => Absent
--   Key in map, no collision => Present v (-1)
--   Key in map, collision    => Present v position
lookupRecordCollision :: Eq k => Hash -> k -> HashMap k v -> LookupRes v
lookupRecordCollision h k m = case lookupRecordCollision# h k m of
  (# (# #) | #) -> Absent
  (# | (# a, i #) #) -> Present a (I# i) -- GHC will eliminate the I#
{-# INLINE lookupRecordCollision #-}

-- Why do we produce an Int# instead of an Int? Unfortunately, GHC is not
-- yet any good at unboxing things *inside* products, let alone sums. That
-- may be changing in GHC 8.6 or so (there is some work in progress), but
-- for now we use Int# explicitly here. We don't need to push the Int#
-- into lookupCont because inlining takes care of that.
lookupRecordCollision# :: Eq k => Hash -> k -> HashMap k v -> (# (# #) | (# v, Int# #) #)
lookupRecordCollision# h k m =
    lookupCont (\_ -> (# (# #) | #)) (\v (I# i) -> (# | (# v, i #) #)) h k 0 m
-- INLINABLE to specialize to the Eq instance.
{-# INLINABLE lookupRecordCollision# #-}

-- A two-continuation version of lookupRecordCollision. This lets us
-- share source code between lookup and lookupRecordCollision without
-- risking any performance degradation.
--
-- The absent continuation has type @((# #) -> r)@ instead of just @r@
-- so we can be representation-polymorphic in the result type. Since
-- this whole thing is always inlined, we don't have to worry about
-- any extra CPS overhead.
--
-- The @Int@ argument is the offset of the subkey in the hash. When looking up
-- keys at the top-level of a hashmap, the offset should be 0. When looking up
-- keys at level @n@ of a hashmap, the offset should be @n * bitsPerSubkey@.
lookupCont ::
  forall rep (r :: TYPE rep) k v.
     Eq k
  => ((# #) -> r)    -- Absent continuation
  -> (v -> Int -> r) -- Present continuation
  -> Hash -- The hash of the key
  -> k
  -> Int -- The offset of the subkey in the hash.
  -> HashMap k v -> r
lookupCont absent present !h0 !k0 !s0 !m0 = go h0 k0 s0 m0
  where
    go :: Eq k => Hash -> k -> Int -> HashMap k v -> r
    go !_ !_ !_ Empty = absent (# #)
    go h k _ (Leaf hx (L kx x))
        | h == hx && k == kx = present x (-1)
        | otherwise          = absent (# #)
    go h k s (BitmapIndexed b v)
        | b .&. m == 0 = absent (# #)
        | otherwise    =
            go h k (nextShift s) (A.index v (sparseIndex b m))
      where m = mask h s
    go h k s (Full v) =
      go h k (nextShift s) (A.index v (index h s))
    go h k _ (Collision hx v)
        | h == hx   = lookupInArrayCont absent present k v
        | otherwise = absent (# #)
{-# INLINE lookupCont #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
--
-- This is a flipped version of 'lookup'.
--
-- @since 0.2.11
(!?) :: (Eq k, Hashable k) => HashMap k v -> k -> Maybe v
(!?) m k = lookup k m
{-# INLINE (!?) #-}


-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
--
-- @since 0.2.11
findWithDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
findWithDefault def k t = case lookup k t of
    Just v -> v
    _      -> def
{-# INLINABLE findWithDefault #-}


-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
--
-- DEPRECATED: lookupDefault is deprecated as of version 0.2.11, replaced
-- by 'findWithDefault'.
lookupDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
lookupDefault = findWithDefault
{-# INLINE lookupDefault #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: (Eq k, Hashable k, HasCallStack) => HashMap k v -> k -> v
(!) m k = case lookup k m of
    Just v  -> v
    Nothing -> error "Data.HashMap.Internal.(!): key not found"
{-# INLINABLE (!) #-}

infixl 9 !

-- | Create a 'Collision' value with two 'Leaf' values.
collision :: Hash -> Leaf k v -> Leaf k v -> HashMap k v
collision h !e1 !e2 =
    let v = A.run $ do mary <- A.new 2 e1
                       A.write mary 1 e2
                       return mary
    in Collision h v
{-# INLINE collision #-}

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> A.Array (HashMap k v) -> HashMap k v
-- The strictness in @ary@ helps achieve a nice code size reduction in
-- @unionWith[Key]@ with GHC 9.2.2. See the Core diffs in
-- https://github.com/haskell-unordered-containers/unordered-containers/pull/376.
bitmapIndexedOrFull b !ary
    | b == fullBitmap = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

-- | \(O(\log n)\) Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k v m = insert' (hash k) k v m
{-# INLINABLE insert #-}

insert' :: Eq k => Hash -> k -> v -> HashMap k v -> HashMap k v
insert' h0 k0 v0 m0 = go h0 k0 v0 0 m0
  where
    go !h !k x !_ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then if x `ptrEq` y
                         then t
                         else Leaf h (L k x)
                    else collision h l (L k x)
        | otherwise = runST (two s h k x hy t)
    go h k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 =
            let !ary' = A.insert ary i $! Leaf h (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st  = A.index ary i
                !st' = go h k x (nextShift s) st
            in if st' `ptrEq` st
               then t
               else BitmapIndexed b (A.update ary i st')
      where m = mask h s
            i = sparseIndex b m
    go h k x s t@(Full ary) =
        let !st  = A.index ary i
            !st' = go h k x (nextShift s) st
        in if st' `ptrEq` st
            then t
            else Full (update32 ary i st')
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = Collision h (updateOrSnocWith (\a _ -> (# a #)) k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
{-# INLINABLE insert' #-}

-- Insert optimized for the case when we know the key is not in the map.
--
-- It is only valid to call this when the key does not exist in the map.
--
-- We can skip:
--  - the key equality check on a Leaf
--  - check for its existence in the array for a hash collision
insertNewKey :: Hash -> k -> v -> HashMap k v -> HashMap k v
insertNewKey !h0 !k0 x0 !m0 = go h0 k0 x0 0 m0
  where
    go !h !k x !_ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l)
      | hy == h = collision h l (L k x)
      | otherwise = runST (two s h k x hy t)
    go h k x s (BitmapIndexed b ary)
        | b .&. m == 0 =
            let !ary' = A.insert ary i $! Leaf h (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st  = A.index ary i
                !st' = go h k x (nextShift s) st
            in BitmapIndexed b (A.update ary i st')
      where m = mask h s
            i = sparseIndex b m
    go h k x s (Full ary) =
        let !st  = A.index ary i
            !st' = go h k x (nextShift s) st
        in Full (update32 ary i st')
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = Collision h (A.snoc v (L k x))
        | otherwise =
            go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
{-# NOINLINE insertNewKey #-}


-- Insert optimized for the case when we know the key is in the map.
--
-- It is only valid to call this when the key exists in the map and you know the
-- hash collision position if there was one. This information can be obtained
-- from 'lookupRecordCollision'. If there is no collision, pass (-1) as collPos
-- (first argument).
insertKeyExists :: Int -> Hash -> k -> v -> HashMap k v -> HashMap k v
insertKeyExists !collPos0 !h0 !k0 x0 !m0 = go collPos0 h0 k0 x0 m0
  where
    go !_collPos !_shiftedHash !k x (Leaf h _kx)
        = Leaf h (L k x)
    go collPos shiftedHash k x (BitmapIndexed b ary) =
        let !st  = A.index ary i
            !st' = go collPos (shiftHash shiftedHash) k x st
        in BitmapIndexed b (A.update ary i st')
      where m = mask' shiftedHash
            i = sparseIndex b m
    go collPos shiftedHash k x (Full ary) =
        let !st  = A.index ary i
            !st' = go collPos (shiftHash shiftedHash) k x st
        in Full (update32 ary i st')
      where i = index' shiftedHash
    go collPos _shiftedHash k x (Collision h v)
        | collPos >= 0 = Collision h (setAtPosition collPos k x v)
        | otherwise = Empty -- error "Internal error: go {collPos negative}"
    go _ _ _ _ Empty = Empty -- error "Internal error: go Empty"

    -- Customized version of 'index' that doesn't require a 'Shift'.
    index' :: Hash -> Int
    index' w = fromIntegral $ w .&. subkeyMask
    {-# INLINE index' #-}

    -- Customized version of 'mask' that doesn't require a 'Shift'.
    mask' :: Word -> Bitmap
    mask' w = 1 `unsafeShiftL` index' w
    {-# INLINE mask' #-}

    shiftHash h = h `unsafeShiftR` bitsPerSubkey
    {-# INLINE shiftHash #-}

{-# NOINLINE insertKeyExists #-}

-- Replace the ith Leaf with Leaf k v.
--
-- This does not check that @i@ is within bounds of the array.
setAtPosition :: Int -> k -> v -> A.Array (Leaf k v) -> A.Array (Leaf k v)
setAtPosition i k x ary = A.update ary i (L k x)
{-# INLINE setAtPosition #-}


-- | In-place update version of insert
unsafeInsert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
unsafeInsert k0 v0 m0 = runST (go h0 k0 v0 0 m0)
  where
    h0 = hash k0
    go !h !k x !_ Empty = return $! Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then if x `ptrEq` y
                         then return t
                         else return $! Leaf h (L k x)
                    else return $! collision h l (L k x)
        | otherwise = two s h k x hy t
    go h k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 = do
            ary' <- A.insertM ary i $! Leaf h (L k x)
            return $! bitmapIndexedOrFull (b .|. m) ary'
        | otherwise = do
            st <- A.indexM ary i
            st' <- go h k x (nextShift s) st
            A.unsafeUpdateM ary i st'
            return t
      where m = mask h s
            i = sparseIndex b m
    go h k x s t@(Full ary) = do
        st <- A.indexM ary i
        st' <- go h k x (nextShift s) st
        A.unsafeUpdateM ary i st'
        return t
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = return $! Collision h (updateOrSnocWith (\a _ -> (# a #)) k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
{-# INLINABLE unsafeInsert #-}

-- | Create a map from two key-value pairs which hashes don't collide. To
-- enhance sharing, the second key-value pair is represented by the hash of its
-- key and a singleton HashMap pairing its key with its value.
--
-- Note: to avoid silly thunks, this function must be strict in the
-- key. See issue #232. We don't need to force the HashMap argument
-- because it's already in WHNF (having just been matched) and we
-- just put it directly in an array.
two :: Shift -> Hash -> k -> v -> Hash -> HashMap k v -> ST s (HashMap k v)
two = go
  where
    go s h1 k1 v1 h2 t2
        | bp1 == bp2 = do
            st <- go (nextShift s) h1 k1 v1 h2 t2
            ary <- A.singletonM st
            return $ BitmapIndexed bp1 ary
        | otherwise  = do
            mary <- A.new 2 $! Leaf h1 (L k1 v1)
            A.write mary idx2 t2
            ary <- A.unsafeFreeze mary
            return $ BitmapIndexed (bp1 .|. bp2) ary
      where
        bp1  = mask h1 s
        bp2  = mask h2 s
        !(I# i1) = index h1 s
        !(I# i2) = index h2 s
        idx2 = I# (i1 Exts.<# i2)
        -- This way of computing idx2 saves us a branch compared to the previous approach:
        --
        -- idx2 | index h1 s < index h2 s = 1
        --      | otherwise               = 0
        --
        -- See https://github.com/haskell-unordered-containers/unordered-containers/issues/75#issuecomment-1128419337
{-# INLINE two #-}

-- | \(O(\log n)\) Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
            -> HashMap k v
-- We're not going to worry about allocating a function closure
-- to pass to insertModifying. See comments at 'adjust'.
insertWith f k new m = insertModifying new (\old -> (# f new old #)) k m
{-# INLINE insertWith #-}

-- | @insertModifying@ is a lot like insertWith; we use it to implement alterF.
-- It takes a value to insert when the key is absent and a function
-- to apply to calculate a new value when the key is present. Thanks
-- to the unboxed unary tuple, we avoid introducing any unnecessary
-- thunks in the tree.
insertModifying :: (Eq k, Hashable k) => v -> (v -> (# v #)) -> k -> HashMap k v
            -> HashMap k v
insertModifying x f k0 m0 = go h0 k0 0 m0
  where
    !h0 = hash k0
    go !h !k !_ Empty = Leaf h (L k x)
    go h k s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then case f y of
                      (# v' #) | ptrEq y v' -> t
                               | otherwise -> Leaf h (L k v')
                    else collision h l (L k x)
        | otherwise = runST (two s h k x hy t)
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 =
            let ary' = A.insert ary i $! Leaf h (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st   = A.index ary i
                !st'  = go h k (nextShift s) st
                ary'  = A.update ary i $! st'
            in if ptrEq st st'
               then t
               else BitmapIndexed b ary'
      where m = mask h s
            i = sparseIndex b m
    go h k s t@(Full ary) =
        let !st   = A.index ary i
            !st'  = go h k (nextShift s) st
            ary' = update32 ary i $! st'
        in if ptrEq st st'
           then t
           else Full ary'
      where i = index h s
    go h k s t@(Collision hy v)
        | h == hy   =
            let !v' = insertModifyingArr x f k v
            in if A.unsafeSameArray v v'
               then t
               else Collision h v'
        | otherwise = go h k s $ BitmapIndexed (mask hy s) (A.singleton t)
{-# INLINABLE insertModifying #-}

-- Like insertModifying for arrays; used to implement insertModifying
insertModifyingArr :: Eq k => v -> (v -> (# v #)) -> k -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
insertModifyingArr x f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
          -- Not found, append to the end.
        | i >= n = A.snoc ary $ L k x
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> case f y of
                                      (# y' #) -> if ptrEq y y'
                                                  then ary
                                                  else A.update ary i (L k y')
                     | otherwise -> go k ary (i+1) n
{-# INLINE insertModifyingArr #-}

-- | In-place update version of insertWith
unsafeInsertWith :: forall k v. (Eq k, Hashable k)
                 => (v -> v -> v) -> k -> v -> HashMap k v
                 -> HashMap k v
unsafeInsertWith f k0 v0 m0 = unsafeInsertWithKey (\_ a b -> (# f a b #)) k0 v0 m0
{-# INLINABLE unsafeInsertWith #-}

unsafeInsertWithKey :: forall k v. (Eq k, Hashable k)
                 => (k -> v -> v -> (# v #)) -> k -> v -> HashMap k v
                 -> HashMap k v
unsafeInsertWithKey f k0 v0 m0 = runST (go h0 k0 v0 0 m0)
  where
    h0 = hash k0
    go :: Hash -> k -> v -> Shift -> HashMap k v -> ST s (HashMap k v)
    go !h !k x !_ Empty = return $! Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then case f k x y of
                        (# v #) -> return $! Leaf h (L k v)
                    else return $! collision h l (L k x)
        | otherwise = two s h k x hy t
    go h k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 = do
            ary' <- A.insertM ary i $! Leaf h (L k x)
            return $! bitmapIndexedOrFull (b .|. m) ary'
        | otherwise = do
            st <- A.indexM ary i
            st' <- go h k x (nextShift s) st
            A.unsafeUpdateM ary i st'
            return t
      where m = mask h s
            i = sparseIndex b m
    go h k x s t@(Full ary) = do
        st <- A.indexM ary i
        st' <- go h k x (nextShift s) st
        A.unsafeUpdateM ary i st'
        return t
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = return $! Collision h (updateOrSnocWithKey f k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
{-# INLINABLE unsafeInsertWithKey #-}

-- | \(O(\log n)\) Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k m = delete' (hash k) k m
{-# INLINABLE delete #-}

delete' :: Eq k => Hash -> k -> HashMap k v -> HashMap k v
delete' h0 k0 m0 = go h0 k0 0 m0
  where
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky _))
        | hy == h && ky == k = Empty
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise =
            let !st = A.index ary i
                !st' = go h k (nextShift s) st
            in if st' `ptrEq` st
                then t
                else case st' of
                Empty | A.length ary == 1 -> Empty
                      | A.length ary == 2 ->
                          case (i, A.index ary 0, A.index ary 1) of
                          (0, _, l) | isLeafOrCollision l -> l
                          (1, l, _) | isLeafOrCollision l -> l
                          _                               -> bIndexed
                      | otherwise -> bIndexed
                    where
                      bIndexed = BitmapIndexed (b .&. complement m) (A.delete ary i)
                l | isLeafOrCollision l && A.length ary == 1 -> l
                _ -> BitmapIndexed b (A.update ary i st')
      where m = mask h s
            i = sparseIndex b m
    go h k s t@(Full ary) =
        let !st   = A.index ary i
            !st' = go h k (nextShift s) st
        in if st' `ptrEq` st
            then t
            else case st' of
            Empty ->
                let ary' = A.delete ary i
                    bm   = fullBitmap .&. complement (1 `unsafeShiftL` i)
                in BitmapIndexed bm ary'
            _ -> Full (A.update ary i st')
      where i = index h s
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
{-# INLINABLE delete' #-}

-- | Delete optimized for the case when we know the key is in the map.
--
-- It is only valid to call this when the key exists in the map and you know the
-- hash collision position if there was one. This information can be obtained
-- from 'lookupRecordCollision'. If there is no collision, pass (-1) as collPos.
deleteKeyExists :: Int -> Hash -> k -> HashMap k v -> HashMap k v
deleteKeyExists !collPos0 !h0 !k0 !m0 = go collPos0 h0 k0 m0
  where
    go :: Int -> Word -> k -> HashMap k v -> HashMap k v
    go !_collPos !_shiftedHash !_k (Leaf _ _) = Empty
    go collPos shiftedHash k (BitmapIndexed b ary) =
            let !st = A.index ary i
                !st' = go collPos (shiftHash shiftedHash) k st
            in case st' of
                Empty | A.length ary == 1 -> Empty
                      | A.length ary == 2 ->
                          case (i, A.index ary 0, A.index ary 1) of
                          (0, _, l) | isLeafOrCollision l -> l
                          (1, l, _) | isLeafOrCollision l -> l
                          _                               -> bIndexed
                      | otherwise -> bIndexed
                    where
                      bIndexed = BitmapIndexed (b .&. complement m) (A.delete ary i)
                l | isLeafOrCollision l && A.length ary == 1 -> l
                _ -> BitmapIndexed b (A.update ary i st')
      where m = mask' shiftedHash
            i = sparseIndex b m
    go collPos shiftedHash k (Full ary) =
        let !st   = A.index ary i
            !st' = go collPos (shiftHash shiftedHash) k st
        in case st' of
            Empty ->
                let ary' = A.delete ary i
                    bm   = fullBitmap .&. complement (1 `unsafeShiftL` i)
                in BitmapIndexed bm ary'
            _ -> Full (A.update ary i st')
      where i = index' shiftedHash
    go collPos _shiftedHash _k (Collision h v)
      | A.length v == 2
      = if collPos == 0
        then Leaf h (A.index v 1)
        else Leaf h (A.index v 0)
      | otherwise = Collision h (A.delete v collPos)
    go !_ !_ !_ Empty = Empty -- error "Internal error: deleteKeyExists empty"

    -- Customized version of 'index' that doesn't require a 'Shift'.
    index' :: Hash -> Int
    index' w = fromIntegral $ w .&. subkeyMask
    {-# INLINE index' #-}

    -- Customized version of 'mask' that doesn't require a 'Shift'.
    mask' :: Word -> Bitmap
    mask' w = 1 `unsafeShiftL` index' w
    {-# INLINE mask' #-}

    shiftHash h = h `unsafeShiftR` bitsPerSubkey
    {-# INLINE shiftHash #-}

{-# NOINLINE deleteKeyExists #-}

-- | \(O(\log n)\) Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
-- This operation really likes to leak memory, so using this
-- indirect implementation shouldn't hurt much. Furthermore, it allows
-- GHC to avoid a leak when the function is lazy. In particular,
--
--     adjust (const x) k m
-- ==> adjust# (\v -> (# const x v #)) k m
-- ==> adjust# (\_ -> (# x #)) k m
adjust f k m = adjust# (\v -> (# f v #)) k m
{-# INLINE adjust #-}

-- | Much like 'adjust', but not inherently leaky.
adjust# :: (Eq k, Hashable k) => (v -> (# v #)) -> k -> HashMap k v -> HashMap k v
adjust# f k0 m0 = go h0 k0 0 m0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky y))
        | hy == h && ky == k = case f y of
            (# y' #) | ptrEq y y' -> t
                     | otherwise -> Leaf h (L k y')
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise = let !st   = A.index ary i
                          !st'  = go h k (nextShift s) st
                          ary' = A.update ary i $! st'
                      in if ptrEq st st'
                         then t
                         else BitmapIndexed b ary'
      where m = mask h s
            i = sparseIndex b m
    go h k s t@(Full ary) =
        let i    = index h s
            !st   = A.index ary i
            !st'  = go h k (nextShift s) st
            ary' = update32 ary i $! st'
        in if ptrEq st st'
           then t
           else Full ary'
    go h k _ t@(Collision hy v)
        | h == hy   = let !v' = updateWith# f k v
                      in if A.unsafeSameArray v v'
                         then t
                         else Collision h v'
        | otherwise = t
{-# INLINABLE adjust# #-}

-- | \(O(\log n)\)  The expression @('update' f k map)@ updates the value @x@ at @k@
-- (if it is in the map). If @(f x)@ is 'Nothing', the element is deleted.
-- If it is @('Just' y)@, the key @k@ is bound to the new value @y@.
update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> HashMap k a -> HashMap k a
update f = alter (>>= f)
{-# INLINABLE update #-}


-- | \(O(\log n)\)  The expression @('alter' f k map)@ alters the value @x@ at @k@, or
-- absence thereof.
--
-- 'alter' can be used to insert, delete, or update a value in a map. In short:
--
-- @
-- 'lookup' k ('alter' f k m) = f ('lookup' k m)
-- @
alter :: (Eq k, Hashable k) => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k m =
    let !h = hash k
        !lookupRes = lookupRecordCollision h k m
    in case f (lookupResToMaybe lookupRes) of
        Nothing -> case lookupRes of
            Absent            -> m
            Present _ collPos -> deleteKeyExists collPos h k m
        Just v' -> case lookupRes of
            Absent            -> insertNewKey h k v' m
            Present v collPos ->
                if v `ptrEq` v'
                    then m
                    else insertKeyExists collPos h k v' m
{-# INLINABLE alter #-}

-- | \(O(\log n)\)  The expression @('alterF' f k map)@ alters the value @x@ at
-- @k@, or absence thereof.
--
--  'alterF' can be used to insert, delete, or update a value in a map.
--
-- Note: 'alterF' is a flipped version of the 'at' combinator from
-- <https://hackage.haskell.org/package/lens/docs/Control-Lens-At.html#v:at Control.Lens.At>.
--
-- @since 0.2.10
alterF :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
-- We only calculate the hash once, but unless this is rewritten
-- by rules we may test for key equality multiple times.
-- We force the value of the map for consistency with the rewritten
-- version; otherwise someone could tell the difference using a lazy
-- @f@ and a functor that is similar to Const but not actually Const.
alterF f = \ !k !m ->
  let
    !h = hash k
    mv = lookup' h k m
  in (<$> f mv) $ \case
    Nothing -> maybe m (const (delete' h k m)) mv
    Just v' -> insert' h k v' m

-- We unconditionally rewrite alterF in RULES, but we expose an
-- unfolding just in case it's used in some way that prevents the
-- rule from firing.
{-# INLINABLE [0] alterF #-}

-- This is just a bottom value. See the comment on the "alterFWeird"
-- rule.
test_bottom :: a
test_bottom = error "Data.HashMap.alterF internal error: hit test_bottom"

-- We use this as an error result in RULES to ensure we don't get
-- any useless CallStack nonsense.
bogus# :: (# #) -> (# a #)
bogus# _ = error "Data.HashMap.alterF internal error: hit bogus#"

{-# RULES
-- We probe the behavior of @f@ by applying it to Nothing and to
-- Just test_bottom. Based on the results, and how they relate to
-- each other, we choose the best implementation.

"alterFWeird" forall f. alterF f =
   alterFWeird (f Nothing) (f (Just test_bottom)) f

-- This rule covers situations where alterF is used to simply insert or
-- delete in Identity (most likely via Control.Lens.At). We recognize here
-- (through the repeated @x@ on the LHS) that
--
-- @f Nothing = f (Just bottom)@,
--
-- which guarantees that @f@ doesn't care what its argument is, so
-- we don't have to either.
--
-- Why only Identity? A variant of this rule is actually valid regardless of
-- the functor, but for some functors (e.g., []), it can lead to the
-- same keys being compared multiple times, which is bad if they're
-- ugly things like strings. This is unfortunate, since the rule is likely
-- a good idea for almost all realistic uses, but I don't like nasty
-- edge cases.
"alterFconstant" forall (f :: Maybe a -> Identity (Maybe a)) x.
  alterFWeird x x f = \ !k !m ->
    Identity (case runIdentity x of {Nothing -> delete k m; Just a -> insert k a m})

-- This rule handles the case where 'alterF' is used to do 'insertWith'-like
-- things. Whenever possible, GHC will get rid of the Maybe nonsense for us.
-- We delay this rule to stage 1 so alterFconstant has a chance to fire.
"alterFinsertWith" [1] forall (f :: Maybe a -> Identity (Maybe a)) x y.
  alterFWeird (coerce (Just x)) (coerce (Just y)) f =
    coerce (insertModifying x (\mold -> case runIdentity (f (Just mold)) of
                                            Nothing -> bogus# (# #)
                                            Just new -> (# new #)))

-- Handle the case where someone uses 'alterF' instead of 'adjust'. This
-- rule is kind of picky; it will only work if the function doesn't
-- do anything between case matching on the Maybe and producing a result.
"alterFadjust" forall (f :: Maybe a -> Identity (Maybe a)) _y.
  alterFWeird (coerce Nothing) (coerce (Just _y)) f =
    coerce (adjust# (\x -> case runIdentity (f (Just x)) of
                               Just x' -> (# x' #)
                               Nothing -> bogus# (# #)))

-- The simple specialization to Const; in this case we can look up
-- the key without caring what position it's in. This is only a tiny
-- optimization.
"alterFlookup" forall _ign1 _ign2 (f :: Maybe a -> Const r (Maybe a)).
  alterFWeird _ign1 _ign2 f = \ !k !m -> Const (getConst (f (lookup k m)))
 #-}

-- This is a very unsafe version of alterF used for RULES. When calling
-- alterFWeird x y f, the following *must* hold:
--
-- x = f Nothing
-- y = f (Just _|_)
--
-- Failure to abide by these laws will make demons come out of your nose.
alterFWeird
       :: (Functor f, Eq k, Hashable k)
       => f (Maybe v)
       -> f (Maybe v)
       -> (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterFWeird _ _ f = alterFEager f
{-# INLINE [0] alterFWeird #-}

-- | This is the default version of alterF that we use in most non-trivial
-- cases. It's called "eager" because it looks up the given key in the map
-- eagerly, whether or not the given function requires that information.
alterFEager :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterFEager f !k m = (<$> f mv) $ \case

    ------------------------------
    -- Delete the key from the map.
    Nothing -> case lookupRes of

      -- Key did not exist in the map to begin with, no-op
      Absent -> m

      -- Key did exist
      Present _ collPos -> deleteKeyExists collPos h k m

    ------------------------------
    -- Update value
    Just v' -> case lookupRes of

      -- Key did not exist before, insert v' under a new key
      Absent -> insertNewKey h k v' m

      -- Key existed before
      Present v collPos ->
        if v `ptrEq` v'
        -- If the value is identical, no-op
        then m
        -- If the value changed, update the value.
        else insertKeyExists collPos h k v' m

  where !h = hash k
        !lookupRes = lookupRecordCollision h k m
        !mv = lookupResToMaybe lookupRes
{-# INLINABLE alterFEager #-}

-- | \(O(n \log m)\) Inclusion of maps. A map is included in another map if the keys
-- are subsets and the corresponding values are equal:
--
-- > isSubmapOf m1 m2 = keys m1 `isSubsetOf` keys m2 &&
-- >                    and [ v1 == v2 | (k1,v1) <- toList m1; let v2 = m2 ! k1 ]
--
-- ==== __Examples__
--
-- >>> fromList [(1,'a')] `isSubmapOf` fromList [(1,'a'),(2,'b')]
-- True
--
-- >>> fromList [(1,'a'),(2,'b')] `isSubmapOf` fromList [(1,'a')]
-- False
--
-- @since 0.2.12
isSubmapOf :: (Eq k, Hashable k, Eq v) => HashMap k v -> HashMap k v -> Bool
isSubmapOf = Exts.inline isSubmapOfBy (==)
{-# INLINABLE isSubmapOf #-}

-- | \(O(n \log m)\) Inclusion of maps with value comparison. A map is included in
-- another map if the keys are subsets and if the comparison function is true
-- for the corresponding values:
--
-- > isSubmapOfBy cmpV m1 m2 = keys m1 `isSubsetOf` keys m2 &&
-- >                           and [ v1 `cmpV` v2 | (k1,v1) <- toList m1; let v2 = m2 ! k1 ]
--
-- ==== __Examples__
--
-- >>> isSubmapOfBy (<=) (fromList [(1,'a')]) (fromList [(1,'b'),(2,'c')])
-- True
--
-- >>> isSubmapOfBy (<=) (fromList [(1,'b')]) (fromList [(1,'a'),(2,'c')])
-- False
--
-- @since 0.2.12
isSubmapOfBy :: (Eq k, Hashable k) => (v1 -> v2 -> Bool) -> HashMap k v1 -> HashMap k v2 -> Bool
-- For maps without collisions the complexity is O(n*log m), where n is the size
-- of m1 and m the size of m2: the inclusion operation visits every leaf in m1 at least once.
-- For each leaf in m1, it looks up the key in m2.
--
-- The worst case complexity is O(n*m). The worst case is when both hashmaps m1
-- and m2 are collision nodes for the same hash. Since collision nodes are
-- unsorted arrays, it requires for every key in m1 a linear search to to find a
-- matching key in m2, hence O(n*m).
isSubmapOfBy comp !m1 !m2 = go 0 m1 m2
  where
    -- An empty map is always a submap of any other map.
    go _ Empty _ = True

    -- If the second map is empty and the first is not, it cannot be a submap.
    go _ _ Empty = False

    -- If the first map contains only one entry, lookup the key in the second map.
    go s (Leaf h1 (L k1 v1)) t2 = lookupCont (\_ -> False) (\v2 _ -> comp v1 v2) h1 k1 s t2

    -- In this case, we need to check that for each x in ls1, there is a y in
    -- ls2 such that x `comp` y. This is the worst case complexity-wise since it
    -- requires a O(m*n) check.
    go _ (Collision h1 ls1) (Collision h2 ls2) =
      h1 == h2 && subsetArray comp ls1 ls2

    -- In this case, we only need to check the entries in ls2 with the hash h1.
    go s t1@(Collision h1 _) (BitmapIndexed b ls2)
        | b .&. m == 0 = False
        | otherwise    =
            go (nextShift s) t1 (A.index ls2 (sparseIndex b m))
      where m = mask h1 s

    -- Similar to the previous case we need to traverse l2 at the index for the hash h1.
    go s t1@(Collision h1 _) (Full ls2) =
      go (nextShift s) t1 (A.index ls2 (index h1 s))

    -- In cases where the first and second map are BitmapIndexed or Full,
    -- traverse down the tree at the appropriate indices.
    go s (BitmapIndexed b1 ls1) (BitmapIndexed b2 ls2) =
      submapBitmapIndexed (go (nextShift s)) b1 ls1 b2 ls2
    go s (BitmapIndexed b1 ls1) (Full ls2) =
      submapBitmapIndexed (go (nextShift s)) b1 ls1 fullBitmap ls2
    go s (Full ls1) (Full ls2) =
      submapBitmapIndexed (go (nextShift s)) fullBitmap ls1 fullBitmap ls2

    -- Collision and Full nodes always contain at least two entries. Hence it
    -- cannot be a map of a leaf.
    go _ (Collision {}) (Leaf {}) = False
    go _ (BitmapIndexed {}) (Leaf {}) = False
    go _ (Full {}) (Leaf {}) = False
    go _ (BitmapIndexed {}) (Collision {}) = False
    go _ (Full {}) (Collision {}) = False
    go _ (Full {}) (BitmapIndexed {}) = False
{-# INLINABLE isSubmapOfBy #-}

-- | \(O(\min n m))\) Checks if a bitmap indexed node is a submap of another.
submapBitmapIndexed :: (HashMap k v1 -> HashMap k v2 -> Bool) -> Bitmap -> A.Array (HashMap k v1) -> Bitmap -> A.Array (HashMap k v2) -> Bool
submapBitmapIndexed comp !b1 !ary1 !b2 !ary2 = subsetBitmaps && go 0 0 (b1Orb2 .&. negate b1Orb2)
  where
    go :: Int -> Int -> Bitmap -> Bool
    go !i !j !m
      | m > b1Orb2 = True

      -- In case a key is both in ary1 and ary2, check ary1[i] <= ary2[j] and
      -- increment the indices i and j.
      | b1Andb2 .&. m /= 0 = comp (A.index ary1 i) (A.index ary2 j) &&
                             go (i+1) (j+1) (m `unsafeShiftL` 1)

      -- In case a key occurs in ary1, but not ary2, only increment index j.
      | b2 .&. m /= 0 = go i (j+1) (m `unsafeShiftL` 1)

      -- In case a key neither occurs in ary1 nor ary2, continue.
      | otherwise = go i j (m `unsafeShiftL` 1)

    b1Andb2 = b1 .&. b2
    b1Orb2  = b1 .|. b2
    subsetBitmaps = b1Orb2 == b2
{-# INLINABLE submapBitmapIndexed #-}

------------------------------------------------------------------------
-- * Combine

-- | \(O(n+m)\) The union of two maps. If a key occurs in both maps, the
-- mapping from the first will be the mapping in the result.
--
-- ==== __Examples__
--
-- >>> union (fromList [(1,'a'),(2,'b')]) (fromList [(2,'c'),(3,'d')])
-- fromList [(1,'a'),(2,'b'),(3,'d')]
union :: Eq k => HashMap k v -> HashMap k v -> HashMap k v
union = unionWith const
{-# INLINABLE union #-}

-- | \(O(n+m)\) The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the
-- result.
unionWith :: Eq k => (v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWith f = unionWithKey (const f)
{-# INLINE unionWith #-}

-- | \(O(n+m)\) The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the
-- result.
unionWithKey :: Eq k => (k -> v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWithKey f = go 0
  where
    -- empty vs. anything
    go !_ t1 Empty = t1
    go _ Empty t2 = t2
    -- leaf vs. leaf
    go s t1@(Leaf h1 l1@(L k1 v1)) t2@(Leaf h2 l2@(L k2 v2))
        | h1 == h2  = if k1 == k2
                      then Leaf h1 (L k1 (f k1 v1 v2))
                      else collision h1 l1 l2
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s t1@(Leaf h1 (L k1 v1)) t2@(Collision h2 ls2)
        | h1 == h2  = Collision h1 (updateOrSnocWithKey (\k a b -> (# f k a b #)) k1 v1 ls2)
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s t1@(Collision h1 ls1) t2@(Leaf h2 (L k2 v2))
        | h1 == h2  = Collision h1 (updateOrSnocWithKey (\k a b -> (# f k b a #)) k2 v2 ls1)
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s t1@(Collision h1 ls1) t2@(Collision h2 ls2)
        | h1 == h2  = Collision h1 (updateOrConcatWithKey (\k a b -> (# f k a b #)) ls1 ls2)
        | otherwise = goDifferentHash s h1 h2 t1 t2
    -- branch vs. branch
    go s (BitmapIndexed b1 ary1) (BitmapIndexed b2 ary2) =
        let b'   = b1 .|. b2
            ary' = unionArrayBy (go (nextShift s)) b1 b2 ary1 ary2
        in bitmapIndexedOrFull b' ary'
    go s (BitmapIndexed b1 ary1) (Full ary2) =
        let ary' = unionArrayBy (go (nextShift s)) b1 fullBitmap ary1 ary2
        in Full ary'
    go s (Full ary1) (BitmapIndexed b2 ary2) =
        let ary' = unionArrayBy (go (nextShift s)) fullBitmap b2 ary1 ary2
        in Full ary'
    go s (Full ary1) (Full ary2) =
        let ary' = unionArrayBy (go (nextShift s)) fullBitmap fullBitmap
                   ary1 ary2
        in Full ary'
    -- leaf vs. branch
    go s (BitmapIndexed b1 ary1) t2
        | b1 .&. m2 == 0 = let ary' = A.insert ary1 i t2
                               b'   = b1 .|. m2
                           in bitmapIndexedOrFull b' ary'
        | otherwise      = let ary' = A.updateWith' ary1 i $ \st1 ->
                                   go (nextShift s) st1 t2
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
                                   go (nextShift s) t1 st2
                           in BitmapIndexed b2 ary'
      where
        h1 = leafHashCode t1
        m1 = mask h1 s
        i = sparseIndex b2 m1
    go s (Full ary1) t2 =
        let h2   = leafHashCode t2
            i    = index h2 s
            ary' = update32With' ary1 i $ \st1 -> go (nextShift s) st1 t2
        in Full ary'
    go s t1 (Full ary2) =
        let h1   = leafHashCode t1
            i    = index h1 s
            ary' = update32With' ary2 i $ \st2 -> go (nextShift s) t1 st2
        in Full ary'

    leafHashCode (Leaf h _) = h
    leafHashCode (Collision h _) = h
    leafHashCode _ = error "leafHashCode"

    goDifferentHash s h1 h2 t1 t2
        | m1 == m2  = BitmapIndexed m1 (A.singleton $! goDifferentHash (nextShift s) h1 h2 t1 t2)
        | m1 <  m2  = BitmapIndexed (m1 .|. m2) (A.pair t1 t2)
        | otherwise = BitmapIndexed (m1 .|. m2) (A.pair t2 t1)
      where
        m1 = mask h1 s
        m2 = mask h2 s
{-# INLINE unionWithKey #-}

-- | Strict in the result of @f@.
unionArrayBy :: (a -> a -> a) -> Bitmap -> Bitmap -> A.Array a -> A.Array a
             -> A.Array a
-- The manual forcing of @b1@, @b2@, @ary1@ and @ary2@ results in handsome
-- Core size reductions with GHC 9.2.2. See the Core diffs in
-- https://github.com/haskell-unordered-containers/unordered-containers/pull/376.
unionArrayBy f !b1 !b2 !ary1 !ary2 = A.run $ do
    let bCombined = b1 .|. b2
    mary <- A.new_ (popCount bCombined)
    -- iterate over nonzero bits of b1 .|. b2
    let go !i !i1 !i2 !b
            | b == 0 = return ()
            | testBit (b1 .&. b2) = do
                x1 <- A.indexM ary1 i1
                x2 <- A.indexM ary2 i2
                A.write mary i $! f x1 x2
                go (i+1) (i1+1) (i2+1) b'
            | testBit b1 = do
                A.write mary i =<< A.indexM ary1 i1
                go (i+1) (i1+1) i2 b'
            | otherwise = do
                A.write mary i =<< A.indexM ary2 i2
                go (i+1) i1 (i2+1) b'
          where
            m = 1 `unsafeShiftL` countTrailingZeros b
            testBit x = x .&. m /= 0
            b' = b .&. complement m
    go 0 0 0 bCombined
    return mary
    -- TODO: For the case where b1 .&. b2 == b1, i.e. when one is a
    -- subset of the other, we could use a slightly simpler algorithm,
    -- where we copy one array, and then update.
{-# INLINE unionArrayBy #-}

-- TODO: Figure out the time complexity of 'unions'.

-- | Construct a set containing all elements from a list of sets.
unions :: Eq k => [HashMap k v] -> HashMap k v
unions = List.foldl' union empty
{-# INLINE unions #-}


------------------------------------------------------------------------
-- * Compose

-- | Relate the keys of one map to the values of
-- the other, by using the values of the former as keys for lookups
-- in the latter.
--
-- Complexity: \( O (n * \log(m)) \), where \(m\) is the size of the first argument
--
-- >>> compose (fromList [('a', "A"), ('b', "B")]) (fromList [(1,'a'),(2,'b'),(3,'z')])
-- fromList [(1,"A"),(2,"B")]
--
-- @
-- ('compose' bc ab '!?') = (bc '!?') <=< (ab '!?')
-- @
--
-- @since 0.2.13.0
compose :: (Eq b, Hashable b) => HashMap b c -> HashMap a b -> HashMap a c
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab

------------------------------------------------------------------------
-- * Transformations

-- | \(O(n)\) Transform this map by applying a function to every value.
mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
mapWithKey f = go
  where
    go Empty = Empty
    go (Leaf h (L k v)) = Leaf h $ L k (f k v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map go ary
    go (Full ary) = Full $ A.map go ary
    -- Why map strictly over collision arrays? Because there's no
    -- point suspending the O(1) work this does for each leaf.
    go (Collision h ary) = Collision h $
                           A.map' (\ (L k v) -> L k (f k v)) ary
{-# INLINE mapWithKey #-}

-- | \(O(n)\) Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = mapWithKey (const f)
{-# INLINE map #-}

-- | \(O(n)\) Perform an 'Applicative' action for each key-value pair
-- in a 'HashMap' and produce a 'HashMap' of all the results.
--
-- Note: the order in which the actions occur is unspecified. In particular,
-- when the map contains hash collisions, the order in which the actions
-- associated with the keys involved will depend in an unspecified way on
-- their insertion order.
traverseWithKey
  :: Applicative f
  => (k -> v1 -> f v2)
  -> HashMap k v1 -> f (HashMap k v2)
traverseWithKey f = go
  where
    go Empty                 = pure Empty
    go (Leaf h (L k v))      = Leaf h . L k <$> f k v
    go (BitmapIndexed b ary) = BitmapIndexed b <$> A.traverse go ary
    go (Full ary)            = Full <$> A.traverse go ary
    go (Collision h ary)     =
        Collision h <$> A.traverse' (\ (L k v) -> L k <$> f k v) ary
{-# INLINE traverseWithKey #-}

-- | \(O(n)\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key. In this case there is no guarantee which of the
-- associated values is chosen for the conflicting key.
--
-- >>> mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])
-- fromList [(4,"b"),(6,"a")]
-- >>> mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")])
-- fromList [(1,"c")]
-- >>> mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")])
-- fromList [(3,"c")]
--
-- @since 0.2.14.0
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []

------------------------------------------------------------------------
-- * Difference and intersection

-- | \(O(n \log m)\) Difference of two maps. Return elements of the first map
-- not existing in the second.
difference :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> unsafeInsert k v m
                 _       -> m
{-# INLINABLE difference #-}

-- | \(O(n \log m)\) Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWith :: (Eq k, Hashable k) => (v -> w -> Maybe v) -> HashMap k v -> HashMap k w -> HashMap k v
differenceWith f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> unsafeInsert k v m
                 Just w  -> maybe m (\y -> unsafeInsert k y m) (f v w)
{-# INLINABLE differenceWith #-}

-- | \(O(n \log m)\) Intersection of two maps. Return elements of the first
-- map for keys existing in the second.
intersection :: Eq k => HashMap k v -> HashMap k w -> HashMap k v
intersection = Exts.inline intersectionWith const
{-# INLINABLE intersection #-}

-- | \(O(n \log m)\) Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWith :: Eq k => (v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWith f = Exts.inline intersectionWithKey $ const f
{-# INLINABLE intersectionWith #-}

-- | \(O(n \log m)\) Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWithKey :: Eq k => (k -> v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey f = intersectionWithKey# $ \k v1 v2 -> (# f k v1 v2 #)
{-# INLINABLE intersectionWithKey #-}

intersectionWithKey# :: Eq k => (k -> v1 -> v2 -> (# v3 #)) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey# f = go 0
  where
    -- empty vs. anything
    go !_ _ Empty = Empty
    go _ Empty _ = Empty
    -- leaf vs. anything
    go s (Leaf h1 (L k1 v1)) t2 =
      lookupCont
        (\_ -> Empty)
        (\v _ -> case f k1 v1 v of (# v' #) -> Leaf h1 $ L k1 v')
        h1 k1 s t2
    go s t1 (Leaf h2 (L k2 v2)) =
      lookupCont
        (\_ -> Empty)
        (\v _ -> case f k2 v v2 of (# v' #) -> Leaf h2 $ L k2 v')
        h2 k2 s t1
    -- collision vs. collision
    go _ (Collision h1 ls1) (Collision h2 ls2) = intersectionCollisions f h1 h2 ls1 ls2
    -- branch vs. branch
    go s (BitmapIndexed b1 ary1) (BitmapIndexed b2 ary2) =
      intersectionArrayBy (go (nextShift s)) b1 b2 ary1 ary2
    go s (BitmapIndexed b1 ary1) (Full ary2) =
      intersectionArrayBy (go (nextShift s)) b1 fullBitmap ary1 ary2
    go s (Full ary1) (BitmapIndexed b2 ary2) =
      intersectionArrayBy (go (nextShift s)) fullBitmap b2 ary1 ary2
    go s (Full ary1) (Full ary2) =
      intersectionArrayBy (go (nextShift s)) fullBitmap fullBitmap ary1 ary2
    -- collision vs. branch
    go s (BitmapIndexed b1 ary1) t2@(Collision h2 _ls2)
      | b1 .&. m2 == 0 = Empty
      | otherwise = go (nextShift s) (A.index ary1 i) t2
      where
        m2 = mask h2 s
        i = sparseIndex b1 m2
    go s t1@(Collision h1 _ls1) (BitmapIndexed b2 ary2)
      | b2 .&. m1 == 0 = Empty
      | otherwise = go (nextShift s) t1 (A.index ary2 i)
      where
        m1 = mask h1 s
        i = sparseIndex b2 m1
    go s (Full ary1) t2@(Collision h2 _ls2) = go (nextShift s) (A.index ary1 i) t2
      where
        i = index h2 s
    go s t1@(Collision h1 _ls1) (Full ary2) = go (nextShift s) t1 (A.index ary2 i)
      where
        i = index h1 s
{-# INLINE intersectionWithKey# #-}

intersectionArrayBy ::
  ( HashMap k v1 ->
    HashMap k v2 ->
    HashMap k v3
  ) ->
  Bitmap ->
  Bitmap ->
  A.Array (HashMap k v1) ->
  A.Array (HashMap k v2) ->
  HashMap k v3
intersectionArrayBy f !b1 !b2 !ary1 !ary2
  | b1 .&. b2 == 0 = Empty
  | otherwise = runST $ do
    mary <- A.new_ $ popCount bIntersect
    -- iterate over nonzero bits of b1 .|. b2
    let go !i !i1 !i2 !b !bFinal
          | b == 0 = pure (i, bFinal)
          | testBit $ b1 .&. b2 = do
            x1 <- A.indexM ary1 i1
            x2 <- A.indexM ary2 i2
            case f x1 x2 of
              Empty -> go i (i1 + 1) (i2 + 1) b' (bFinal .&. complement m)
              _ -> do
                A.write mary i $! f x1 x2
                go (i + 1) (i1 + 1) (i2 + 1) b' bFinal
          | testBit b1 = go i (i1 + 1) i2 b' bFinal
          | otherwise = go i i1 (i2 + 1) b' bFinal
          where
            m = 1 `unsafeShiftL` countTrailingZeros b
            testBit x = x .&. m /= 0
            b' = b .&. complement m
    (len, bFinal) <- go 0 0 0 bCombined bIntersect
    case len of
      0 -> pure Empty
      1 -> do
        l <- A.read mary 0
        if isLeafOrCollision l
          then pure l
          else BitmapIndexed bFinal <$> (A.unsafeFreeze =<< A.shrink mary 1)
      _ -> bitmapIndexedOrFull bFinal <$> (A.unsafeFreeze =<< A.shrink mary len)
  where
    bCombined = b1 .|. b2
    bIntersect = b1 .&. b2
{-# INLINE intersectionArrayBy #-}

intersectionCollisions :: Eq k => (k -> v1 -> v2 -> (# v3 #)) -> Hash -> Hash -> A.Array (Leaf k v1) -> A.Array (Leaf k v2) -> HashMap k v3
intersectionCollisions f h1 h2 ary1 ary2
  | h1 == h2 = runST $ do
    mary2 <- A.thaw ary2 0 $ A.length ary2
    mary <- A.new_ $ min (A.length ary1) (A.length ary2)
    let go i j
          | i >= A.length ary1 || j >= A.lengthM mary2 = pure j
          | otherwise = do
            L k1 v1 <- A.indexM ary1 i
            searchSwap k1 j mary2 >>= \case
              Just (L _k2 v2) -> do
                let !(# v3 #) = f k1 v1 v2
                A.write mary j $ L k1 v3
                go (i + 1) (j + 1)
              Nothing -> do
                go (i + 1) j
    len <- go 0 0
    case len of
      0 -> pure Empty
      1 -> Leaf h1 <$> A.read mary 0
      _ -> Collision h1 <$> (A.unsafeFreeze =<< A.shrink mary len)
  | otherwise = Empty
{-# INLINE intersectionCollisions #-}

-- | Say we have
-- @
-- 1 2 3 4
-- @
-- and we search for @3@. Then we can mutate the array to
-- @
-- undefined 2 1 4
-- @
-- We don't actually need to write undefined, we just have to make sure that the next search starts 1 after the current one.
searchSwap :: Eq k => k -> Int -> A.MArray s (Leaf k v) -> ST s (Maybe (Leaf k v))
searchSwap toFind start = go start toFind start
  where
    go i0 k i mary
      | i >= A.lengthM mary = pure Nothing
      | otherwise = do
        l@(L k' _v) <- A.read mary i
        if k == k'
          then do
            A.write mary i =<< A.read mary i0
            pure $ Just l
          else go i0 k (i + 1) mary
{-# INLINE searchSwap #-}

------------------------------------------------------------------------
-- * Folds

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> HashMap k v -> a
foldl' f = foldlWithKey' (\ z _ v -> f z v)
{-# INLINE foldl' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: (v -> a -> a) -> a -> HashMap k v -> a
foldr' f = foldrWithKey' (\ _ v z -> f v z)
{-# INLINE foldr' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' f = go
  where
    go !z Empty                = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl' go z ary
    go z (Full ary)            = A.foldl' go z ary
    go z (Collision _ ary)     = A.foldl' (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithKey' :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey' f = flip go
  where
    go Empty z                 = z
    go (Leaf _ (L k v)) !z     = f k v z
    go (BitmapIndexed _ ary) !z = A.foldr' go z ary
    go (Full ary) !z           = A.foldr' go z ary
    go (Collision _ ary) !z    = A.foldr' (\ (L k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> HashMap k v -> a
foldr f = foldrWithKey (const f)
{-# INLINE foldr #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldl :: (a -> v -> a) -> a -> HashMap k v -> a
foldl f = foldlWithKey (\a _k v -> f a v)
{-# INLINE foldl #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f = flip go
  where
    go Empty z                 = z
    go (Leaf _ (L k v)) z      = f k v z
    go (BitmapIndexed _ ary) z = A.foldr go z ary
    go (Full ary) z            = A.foldr go z ary
    go (Collision _ ary) z     = A.foldr (\ (L k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldlWithKey :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey f = go
  where
    go z Empty                 = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl go z ary
    go z (Full ary)            = A.foldl go z ary
    go z (Collision _ ary)     = A.foldl (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey #-}

-- | \(O(n)\) Reduce the map by applying a function to each element
-- and combining the results with a monoid operation.
foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldMapWithKey f = go
  where
    go Empty = mempty
    go (Leaf _ (L k v)) = f k v
    go (BitmapIndexed _ ary) = A.foldMap go ary
    go (Full ary) = A.foldMap go ary
    go (Collision _ ary) = A.foldMap (\ (L k v) -> f k v) ary
{-# INLINE foldMapWithKey #-}

------------------------------------------------------------------------
-- * Filter

-- | \(O(n)\) Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybeWithKey :: (k -> v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybeWithKey f = filterMapAux onLeaf onColl
  where onLeaf (Leaf h (L k v)) | Just v' <- f k v = Just (Leaf h (L k v'))
        onLeaf _ = Nothing

        onColl (L k v) | Just v' <- f k v = Just (L k v')
                       | otherwise = Nothing
{-# INLINE mapMaybeWithKey #-}

-- | \(O(n)\) Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | \(O(n)\) Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: forall k v. (k -> v -> Bool) -> HashMap k v -> HashMap k v
filterWithKey pred = filterMapAux onLeaf onColl
  where onLeaf t@(Leaf _ (L k v)) | pred k v = Just t
        onLeaf _ = Nothing

        onColl el@(L k v) | pred k v = Just el
        onColl _ = Nothing
{-# INLINE filterWithKey #-}


-- | Common implementation for 'filterWithKey' and 'mapMaybeWithKey',
--   allowing the former to former to reuse terms.
filterMapAux :: forall k v1 v2
              . (HashMap k v1 -> Maybe (HashMap k v2))
             -> (Leaf k v1 -> Maybe (Leaf k v2))
             -> HashMap k v1
             -> HashMap k v2
filterMapAux onLeaf onColl = go
  where
    go Empty = Empty
    go t@Leaf{}
        | Just t' <- onLeaf t = t'
        | otherwise = Empty
    go (BitmapIndexed b ary) = filterA ary b
    go (Full ary) = filterA ary fullBitmap
    go (Collision h ary) = filterC ary h

    filterA ary0 b0 =
        let !n = A.length ary0
        in runST $ do
            mary <- A.new_ n
            step ary0 mary b0 0 0 1 n
      where
        step :: A.Array (HashMap k v1) -> A.MArray s (HashMap k v2)
             -> Bitmap -> Int -> Int -> Bitmap -> Int
             -> ST s (HashMap k v2)
        step !ary !mary !b i !j !bi n
            | i >= n = case j of
                0 -> return Empty
                1 -> do
                    ch <- A.read mary 0
                    case ch of
                      t | isLeafOrCollision t -> return t
                      _                       -> BitmapIndexed b <$> (A.unsafeFreeze =<< A.shrink mary 1)
                _ -> do
                    ary2 <- A.unsafeFreeze =<< A.shrink mary j
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
        step :: A.Array (Leaf k v1) -> A.MArray s (Leaf k v2)
             -> Int -> Int -> Int
             -> ST s (HashMap k v2)
        step !ary !mary i !j n
            | i >= n    = case j of
                0 -> return Empty
                1 -> do l <- A.read mary 0
                        return $! Leaf h l
                _ | i == j -> do ary2 <- A.unsafeFreeze mary
                                 return $! Collision h ary2
                  | otherwise -> do ary2 <- A.unsafeFreeze =<< A.shrink mary j
                                    return $! Collision h ary2
            | Just el <- onColl $! A.index ary i
                = A.write mary j el >> step ary mary (i+1) (j+1) n
            | otherwise = step ary mary (i+1) j n
{-# INLINE filterMapAux #-}

-- | \(O(n)\) Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (v -> Bool) -> HashMap k v -> HashMap k v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

------------------------------------------------------------------------
-- * Conversions

-- TODO: Improve fusion rules by modelled them after the Prelude ones
-- on lists.

-- | \(O(n)\) Return a list of this map's keys.  The list is produced
-- lazily.
keys :: HashMap k v -> [k]
keys = List.map fst . toList
{-# INLINE keys #-}

-- | \(O(n)\) Return a list of this map's values.  The list is produced
-- lazily.
elems :: HashMap k v -> [v]
elems = List.map snd . toList
{-# INLINE elems #-}

------------------------------------------------------------------------
-- ** Lists

-- | \(O(n)\) Return a list of this map's elements.  The list is
-- produced lazily. The order of its elements is unspecified.
toList :: HashMap k v -> [(k, v)]
toList t = Exts.build (\ c z -> foldrWithKey (curry c) z t)
{-# INLINE toList #-}

-- | \(O(n)\) Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = List.foldl' (\ m (k, v) -> unsafeInsert k v m) empty
{-# INLINABLE fromList #-}

-- | \(O(n \log n)\) Construct a map from a list of elements.  Uses
-- the provided function @f@ to merge duplicate entries with
-- @(f newVal oldVal)@.
--
-- === Examples
--
-- Given a list @xs@, create a map with the number of occurrences of each
-- element in @xs@:
--
-- > let xs = ['a', 'b', 'a']
-- > in fromListWith (+) [ (x, 1) | x <- xs ]
-- >
-- > = fromList [('a', 2), ('b', 1)]
--
-- Given a list of key-value pairs @xs :: [(k, v)]@, group all values by their
-- keys and return a @HashMap k [v]@.
--
-- > let xs = [('a', 1), ('b', 2), ('a', 3)]
-- > in fromListWith (++) [ (k, [v]) | (k, v) <- xs ]
-- >
-- > = fromList [('a', [3, 1]), ('b', [2])]
--
-- Note that the lists in the resulting map contain elements in reverse order
-- from their occurrences in the original list.
--
-- More generally, duplicate entries are accumulated as follows;
-- this matters when @f@ is not commutative or not associative.
--
-- > fromListWith f [(k, a), (k, b), (k, c), (k, d)]
-- > = fromList [(k, f d (f c (f b a)))]
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = List.foldl' (\ m (k, v) -> unsafeInsertWith f k v m) empty
{-# INLINE fromListWith #-}

-- | \(O(n \log n)\) Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
--
-- === Examples
--
-- Given a list of key-value pairs where the keys are of different flavours, e.g:
--
-- > data Key = Div | Sub
--
-- and the values need to be combined differently when there are duplicates,
-- depending on the key:
--
-- > combine Div = div
-- > combine Sub = (-)
--
-- then @fromListWithKey@ can be used as follows:
--
-- > fromListWithKey combine [(Div, 2), (Div, 6), (Sub, 2), (Sub, 3)]
-- > = fromList [(Div, 3), (Sub, 1)]
--
-- More generally, duplicate entries are accumulated as follows;
--
-- > fromListWith f [(k, a), (k, b), (k, c), (k, d)]
-- > = fromList [(k, f k d (f k c (f k b a)))]
--
-- @since 0.2.11
fromListWithKey :: (Eq k, Hashable k) => (k -> v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWithKey f = List.foldl' (\ m (k, v) -> unsafeInsertWithKey (\k' a b -> (# f k' a b #)) k v m) empty
{-# INLINE fromListWithKey #-}

------------------------------------------------------------------------
-- Array operations

-- | \(O(n)\) Look up the value associated with the given key in an
-- array.
lookupInArrayCont ::
  forall rep (r :: TYPE rep) k v.
  Eq k => ((# #) -> r) -> (v -> Int -> r) -> k -> A.Array (Leaf k v) -> r
lookupInArrayCont absent present k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go :: Eq k => k -> A.Array (Leaf k v) -> Int -> Int -> r
    go !k !ary !i !n
        | i >= n    = absent (# #)
        | otherwise = case A.index ary i of
            (L kx v)
                | k == kx   -> present v i
                | otherwise -> go k ary (i+1) n
{-# INLINE lookupInArrayCont #-}

-- | \(O(n)\) Lookup the value associated with the given key in this
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
{-# INLINABLE indexOf #-}

updateWith# :: Eq k => (v -> (# v #)) -> k -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateWith# f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = ary
        | otherwise = case A.index ary i of
            (L kx y) | k == kx -> case f y of
                          (# y' #)
                             | ptrEq y y' -> ary
                             | otherwise -> A.update ary i (L k y')
                     | otherwise -> go k ary (i+1) n
{-# INLINABLE updateWith# #-}

updateOrSnocWith :: Eq k => (v -> v -> (# v #)) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWith f = updateOrSnocWithKey (const f)
{-# INLINABLE updateOrSnocWith #-}

updateOrSnocWithKey :: Eq k => (k -> v -> v -> (# v #)) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWithKey f k0 v0 ary0 = go k0 v0 ary0 0 (A.length ary0)
  where
    go !k v !ary !i !n
        -- Not found, append to the end.
        | i >= n = A.snoc ary $ L k v
        | L kx y <- A.index ary i
        , k == kx
        , (# v2 #) <- f k v y
            = A.update ary i (L k v2)
        | otherwise
            = go k v ary (i+1) n
{-# INLINABLE updateOrSnocWithKey #-}

updateOrConcatWithKey :: Eq k => (k -> v -> v -> (# v #)) -> A.Array (Leaf k v) -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateOrConcatWithKey f ary1 ary2 = A.run $ do
    -- TODO: instead of mapping and then folding, should we traverse?
    -- We'll have to be careful to avoid allocating pairs or similar.

    -- first: look up the position of each element of ary2 in ary1
    let indices = A.map' (\(L k _) -> indexOf k ary1) ary2
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
                             case f k v1 v2 of (# v3 #) -> A.write mary i1 (L k v3)
                             go iEnd (i2+1)
               Nothing -> do -- key is only in ary2, append to end
                             A.write mary iEnd =<< A.indexM ary2 i2
                             go (iEnd+1) (i2+1)
    go n1 0
    return mary
{-# INLINABLE updateOrConcatWithKey #-}

-- | \(O(n*m)\) Check if the first array is a subset of the second array.
subsetArray :: Eq k => (v1 -> v2 -> Bool) -> A.Array (Leaf k v1) -> A.Array (Leaf k v2) -> Bool
subsetArray cmpV ary1 ary2 = A.length ary1 <= A.length ary2 && A.all inAry2 ary1
  where
    inAry2 (L k1 v1) = lookupInArrayCont (\_ -> False) (\v2 _ -> cmpV v1 v2) k1 ary2
    {-# INLINE inAry2 #-}

------------------------------------------------------------------------
-- Manually unrolled loops

-- | \(O(n)\) Update the element at the given position in this array.
update32 :: A.Array e -> Int -> e -> A.Array e
update32 ary idx b = runST (update32M ary idx b)
{-# INLINE update32 #-}

-- | \(O(n)\) Update the element at the given position in this array.
update32M :: A.Array e -> Int -> e -> ST s (A.Array e)
update32M ary idx b = do
    mary <- clone ary
    A.write mary idx b
    A.unsafeFreeze mary
{-# INLINE update32M #-}

-- | \(O(n)\) Update the element at the given position in this array, by applying a function to it.
update32With' :: A.Array e -> Int -> (e -> e) -> A.Array e
update32With' ary idx f
  | (# x #) <- A.index# ary idx
  = update32 ary idx $! f x
{-# INLINE update32With' #-}

-- | Unsafely clone an array of (2^bitsPerSubkey) elements.  The length of the input
-- array is not checked.
clone :: A.Array e -> ST s (A.MArray s e)
clone ary =
    A.thaw ary 0 (2^bitsPerSubkey)

------------------------------------------------------------------------
-- Bit twiddling

-- TODO: Name this 'bitsPerLevel'?! What is a "subkey"?
-- https://github.com/haskell-unordered-containers/unordered-containers/issues/425

-- | Number of bits that are inspected at each level of the hash tree.
--
-- This constant is named /t/ in the original /Ideal Hash Trees/ paper.
bitsPerSubkey :: Int
bitsPerSubkey = 5

-- | The size of a 'Full' node, i.e. @2 ^ 'bitsPerSubkey'@.
maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey

-- | Bit mask with the lowest 'bitsPerSubkey' bits set, i.e. @0b11111@.
subkeyMask :: Word
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

-- | Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
-- the index into a 'Full' node or into the bitmap of a `BitmapIndexed` node.
--
-- >>> index 0b0010_0010 0
-- 0b0000_0010
index :: Hash -> Shift -> Int
index w s = fromIntegral $ unsafeShiftR w s .&. subkeyMask
{-# INLINE index #-}

-- | Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
-- the bitmap that contains only the 'index' of the hash at this level.
--
-- The result can be used for constructing one-element 'BitmapIndexed' nodes or
-- to check whether a 'BitmapIndexed' node may possibly contain the given 'Hash'.
--
-- >>> mask 0b0010_0010 0
-- 0b0100
mask :: Hash -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}

-- | This array index is computed by counting the number of 1-bits below the
-- 'index' represented by the mask.
--
-- >>> sparseIndex 0b0110_0110 0b0010_0000
-- 2
sparseIndex
    :: Bitmap
    -- ^ Bitmap of a 'BitmapIndexed' node
    -> Bitmap
    -- ^ One-bit 'mask' corresponding to the 'index' of a hash
    -> Int
    -- ^ Index into the array of the 'BitmapIndexed' node
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

-- | A bitmap with the 'maxChildren' least significant bits set, i.e.
-- @0xFF_FF_FF_FF@.
fullBitmap :: Bitmap
-- This needs to use 'shiftL' instead of 'unsafeShiftL', to avoid UB.
-- See issue #412.
fullBitmap = complement (complement 0 `shiftL` maxChildren)
{-# INLINE fullBitmap #-}

-- | Increment a 'Shift' for use at the next deeper level.
nextShift :: Shift -> Shift
nextShift s = s + bitsPerSubkey
{-# INLINE nextShift #-}

------------------------------------------------------------------------
-- Pointer equality

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

------------------------------------------------------------------------
-- IsList instance
instance (Eq k, Hashable k) => Exts.IsList (HashMap k v) where
    type Item (HashMap k v) = (k, v)
    fromList = fromList
    toList   = toList
