{-# LANGUAGE BangPatterns, CPP, PatternGuards, MagicHash, UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Strict
-- Copyright   :  2010-2012 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A map from /hashable/ keys to values.  A map cannot contain
-- duplicate keys; each key can map to at most one value.  A 'HashMap'
-- makes no guarantees as to the order of its elements.
--
-- The implementation is based on /hash array mapped tries/.  A
-- 'HashMap' is often faster than other tree-based set types,
-- especially when key comparison is expensive, as in the case of
-- strings.
--
-- Many operations have a average-case complexity of /O(log n)/.  The
-- implementation uses a large base (i.e. 16) so in practice these
-- operations are constant time.
module Data.HashMap.Strict.Base
    (
      -- * Strictness properties
      -- $strictness

      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , HM.null
    , size
    , HM.member
    , HM.lookup
    , lookupDefault
    , (!)
    , insert
    , insertWith
    , delete
    , adjust
    , update
    , alter
    , alterF

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
    , HM.foldr
    , foldrWithKey

      -- * Filter
    , HM.filter
    , filterWithKey
    , mapMaybe
    , mapMaybeWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor((<$>))
#endif
import qualified Data.List as L
import Data.Hashable (Hashable)
import Prelude hiding (map, lookup)

import qualified Data.HashMap.Array as A
import qualified Data.HashMap.Base as HM
import Data.HashMap.Base hiding (
    alter, alterF, adjust, fromList, fromListWith, insert, insertWith,
    differenceWith, intersectionWith, intersectionWithKey, map, mapWithKey,
    mapMaybe, mapMaybeWithKey, singleton, update, unionWith, unionWithKey,
    unsafeInsertWith)
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity
#endif
import Control.Applicative (Const (..))
import Data.Coerce

-- $strictness
--
-- This module satisfies the following strictness properties:
--
-- 1. Key arguments are evaluated to WHNF;
--
-- 2. Keys and values are evaluated to WHNF before they are stored in
--    the map.

------------------------------------------------------------------------
-- * Construction

-- | /O(1)/ Construct a map with a single element.
singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k !v = HM.singleton k v

------------------------------------------------------------------------
-- * Basic interface

-- | /O(log n)/ Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k !v = HM.insert k v
{-# INLINABLE insert #-}

-- | /O(log n)/ Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
           -> HashMap k v
insertWith f k v m = insertModifying (\_ -> v `seq` (# v #)) (\old -> let !v' = f v old in (# v' #)) k m
{-# INLINE insertWith #-}

-- | In-place update version of insertWith
unsafeInsertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
                 -> HashMap k v
unsafeInsertWith f k v m =
  HM.unsafeInsertModifying (\(# #) -> v `seq` (# v #))
                           (\old -> let !v' = f v old in (# v' #)) k m
{-# INLINE unsafeInsertWith #-}

-- | /O(log n)/ Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k m = adjust# (\ v -> let !fv = f v in (# fv #)) k m
{-# INLINE adjust #-}

-- | /O(log n)/  The expression (@'update' f k map@) updates the value @x@ at @k@,
-- (if it is in the map). If (f k x) is @'Nothing', the element is deleted.
-- If it is (@'Just' y), the key k is bound to the new value y.
update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> HashMap k a -> HashMap k a
update f = alter (>>= f)
{-# INLINABLE update #-}

-- | /O(log n)/  The expression (@'alter' f k map@) alters the value @x@ at @k@, or
-- absence thereof. @alter@ can be used to insert, delete, or update a value in a
-- map. In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Eq k, Hashable k) => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k m =
  case f (HM.lookup k m) of
    Nothing -> delete k m
    Just v  -> insert k v m
{-# INLINABLE alter #-}

-- | /O(log n)/  The expression (@'alterF' f k map@) alters the value @x@ at
-- @k@, or absence thereof. @alterF@ can be used to insert, delete, or update
-- a value in a map.
--
-- Note: 'alterF' is a flipped version of the 'at' combinator from
-- <https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-At.html#v:at Control.Lens.At>.
--
-- @since 0.2.9
alterF :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
-- Special care is taken to only calculate the hash once. When we rewrite
-- with RULES, we also ensure that we only compare the key for equality
-- once. We force the value of the map for consistency with the rewritten
-- version; otherwise someone could tell the difference using a lazy
-- @f@ and a functor that is similar to Const but not actually Const.
alterF f = \ !k !m ->
  let !h = hash k
      mv = lookup' h k m
  in (<$> f mv) $ \fres ->
    case fres of
      Nothing -> delete' h k m
      Just !v' -> insert' h k v' m

-- We rewrite this function unconditionally in RULES, but we expose
-- an unfolding just in case it's used in a context where the rules
-- don't fire.
{-# INLINABLE [0] alterF #-}

#if MIN_VERSION_base(4,8,0)
-- See notes in Data.HashMap.Base
test_bottom :: a
test_bottom = error "Data.HashMap.alterF internal error: hit test_bottom"

bogus# :: (# #) -> (# a #)
bogus# _ = error "Data.HashMap.alterF internal error: hit bogus#"

impossibleAdjust :: a
impossibleAdjust = error "Data.HashMap.alterF internal error: impossible adjust"

{-# RULES

-- See detailed notes on alterF rules in Data.HashMap.Base.

"alterFWeird" forall f. alterF f =
    alterFWeird (f Nothing) (f (Just test_bottom)) f

"alterFconstant" forall (f :: Maybe a -> f (Maybe a)) x.
  alterFWeird x x f = \ !k !m ->
    fmap (\case {Nothing -> delete k m; Just a -> insert k a m}) x

"alterFinsertWith" [1] forall (f :: Maybe a -> Identity (Maybe a)) x y.
  alterFWeird (coerce (Just x)) (coerce (Just y)) f =
    coerce (insertModifying (\_ -> x `seq` (# x #)) (\mold -> case runIdentity (f (Just mold)) of
                                            Nothing -> bogus# (# #)
                                            Just !new -> (# new #)))

-- This rule is written a bit differently than the one for lazy
-- maps because the adjust here is strict. We could write it the
-- same general way anyway, but this seems simpler.
"alterFadjust" forall (f :: Maybe a -> Identity (Maybe a)) x.
  alterFWeird (coerce Nothing) (coerce (Just x)) f =
    coerce (adjust (\a -> case runIdentity (f (Just a)) of
                               Just a' -> a'
                               Nothing -> impossibleAdjust))

"alterFlookup" forall _ign1 _ign2 (f :: Maybe a -> Const r (Maybe a)) .
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
alterFEager f !k !m = (<$> f mv) $ \fres ->
  case fres of

    ------------------------------
    -- Delete the key from the map.
    Nothing -> case lookupRes of

      -- Key did not exist in the map to begin with, no-op
      Absent -> m

      -- Key did exist, no collision
      Present _ collPos -> deleteKeyExists collPos h k m

    ------------------------------
    -- Update value
    Just v' -> case lookupRes of

      -- Key did not exist before, insert v' under a new key
      Absent -> insertNewKey h k v' m

      -- Key existed before, no hash collision
      Present v collPos -> v' `seq`
        if v `ptrEq` v'
        -- If the value is identical, no-op
        then m
        -- If the value changed, update the value.
        else insertKeyExists collPos h k v' m

  where !h = hash k
        !lookupRes = lookupRecordCollision h k m
        !mv = case lookupRes of
          Absent -> Nothing
          Present v _ -> Just v
{-# INLINABLE alterFEager #-}
#endif

------------------------------------------------------------------------
-- * Combine

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWith f = unionWithKey (const f)
{-# INLINE unionWith #-}

-- | /O(n+m)/ The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
unionWithKey :: (Eq k, Hashable k) => (k -> v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWithKey f m = unionWithKey# (\k v1 v2 -> let !v = f k v1 v2 in (# v #)) m
{-# INLINE unionWithKey #-}

------------------------------------------------------------------------
-- * Transformations

-- | /O(n)/ Transform this map by applying a function to every value.
mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
mapWithKey f = go
  where
    go Empty                 = Empty
    go (Leaf h (L k v))      = leaf h k (f k v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map' go ary
    go (Full ary)            = Full $ A.map' go ary
    go (Collision h ary)     =
        Collision h $ A.map' (\ (L k v) -> let !v' = f k v in L k v') ary
{-# INLINE mapWithKey #-}

-- | /O(n)/ Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = mapWithKey (const f)
{-# INLINE map #-}


------------------------------------------------------------------------
-- * Filter

-- | /O(n)/ Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybeWithKey :: (k -> v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybeWithKey f = filterMapAux onLeaf onColl
  where onLeaf (Leaf h (L k v)) | Just v' <- f k v = Just (leaf h k v')
        onLeaf _ = Nothing

        onColl (L k v) | Just v' <- f k v = Just (L k v')
                       | otherwise = Nothing
{-# INLINE mapMaybeWithKey #-}

-- | /O(n)/ Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}


-- TODO: Should we add a strict traverseWithKey?

------------------------------------------------------------------------
-- * Difference and intersection

-- | /O(n*log m)/ Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWith :: (Eq k, Hashable k) => (v -> w -> Maybe v) -> HashMap k v -> HashMap k w -> HashMap k v
differenceWith f a b = foldlWithKey' go empty a
  where
    go m k v = case HM.lookup k b of
                 Nothing -> insert k v m
                 Just w  -> maybe m (\y -> insert k y m) (f v w)
{-# INLINABLE differenceWith #-}

-- | /O(n+m)/ Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWith :: (Eq k, Hashable k) => (v1 -> v2 -> v3) -> HashMap k v1
                 -> HashMap k v2 -> HashMap k v3
intersectionWith f a b = foldlWithKey' go empty a
  where
    go m k v = case HM.lookup k b of
                 Just w -> insert k (f v w) m
                 _      -> m
{-# INLINABLE intersectionWith #-}

-- | /O(n+m)/ Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWithKey :: (Eq k, Hashable k) => (k -> v1 -> v2 -> v3)
                    -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey f a b = foldlWithKey' go empty a
  where
    go m k v = case HM.lookup k b of
                 Just w -> insert k (f k v w) m
                 _      -> m
{-# INLINABLE intersectionWithKey #-}

------------------------------------------------------------------------
-- ** Lists

-- | /O(n*log n)/ Construct a map with the supplied mappings.  If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = L.foldl' (\ m (k, !v) -> HM.unsafeInsert k v m) empty
{-# INLINABLE fromList #-}

-- | /O(n*log n)/ Construct a map from a list of elements.  Uses
-- the provided function f to merge duplicate entries (f newVal oldVal).
--
-- For example:
--
-- > fromListWith (+) [ (x, 1) | x <- xs ]
--
-- will create a map with number of occurrences of each element in xs.
--
-- > fromListWith (++) [ (k, [v]) | (k, v) <- xs ]
--
-- will group all values by their keys in a list 'xs :: [(k, v)]' and
-- return a 'HashMap k [v]'.
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = L.foldl' (\ m (k, v) -> unsafeInsertWith f k v m) empty
{-# INLINE fromListWith #-}

------------------------------------------------------------------------
-- Smart constructors
--
-- These constructors make sure the value is in WHNF before it's
-- inserted into the constructor.

leaf :: Hash -> k -> v -> HashMap k v
leaf h k !v = Leaf h (L k v)
{-# INLINE leaf #-}
