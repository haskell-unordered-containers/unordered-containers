{-# LANGUAGE BangPatterns, CPP #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

------------------------------------------------------------------------
-- |
-- Module      :  Data.FullList.Lazy
-- Copyright   :  2010-2011 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Non-empty lists of key/value pairs.  The lists are strict in the
-- keys and lazy in the values.

module Data.FullList.Lazy
    ( FullList(..)
    , List(..)

      -- * Basic interface
    , size
    , singleton
    , lookup
    , insert
    , delete
    , insertWith
    , adjust

      -- * Combine
      -- * Union
    , union
    , unionWith

      -- * Transformations
    , map
    , traverseWithKey

      -- * Folds
    , foldlWithKey'
    , foldrWithKey

      -- * Filter
    , filterWithKey
      -- * For use by FL.Strict
    , lookupL
    , deleteL
    ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Prelude hiding (lookup, map)

------------------------------------------------------------------------
-- * The 'FullList' type

-- The 'FullList' type has two benefits:
--
--  * it is guaranteed to be non-empty, and
--
--  * it can be unpacked into a data constructor.

-- Invariant: the same key only appears once in a 'FullList'.

-- | A non-empty list of key/value pairs.
data FullList k v = FL !k v !(List k v)
                  deriving Show

instance (Eq k, Eq v) => Eq (FullList k v) where
    (FL k1 v1 xs) == (FL k2 v2 ys) = k1 == k2 && v1 == v2 && xs == ys
    (FL k1 v1 xs) /= (FL k2 v2 ys) = k1 /= k2 || v1 /= v2 || xs /= ys

instance (NFData k, NFData v) => NFData (FullList k v)

data List k v = Nil | Cons !k v !(List k v)
              deriving Show

instance (Eq k, Eq v) => Eq (List k v) where
    (Cons k1 v1 xs) == (Cons k2 v2 ys) = k1 == k2 && v1 == v2 && xs == ys
    Nil == Nil = True
    _   == _   = False

    (Cons k1 v1 xs) /= (Cons k2 v2 ys) = k1 /= k2 || v1 /= v2 || xs /= ys
    Nil /= Nil = False
    _   /= _   = True

instance (NFData k, NFData v) => NFData (List k v) where
    rnf Nil           = ()
    rnf (Cons k v xs) = rnf k `seq` rnf v `seq` rnf xs

-- TODO: Check if evaluation is forced.

------------------------------------------------------------------------
-- * FullList

-- The 'List' functions are not inlined as they should be seldomly
-- called in practice (i.e. we expect few collisions.)

size :: FullList k v -> Int
size (FL _ _ xs) = 1 + sizeL xs

sizeL :: List k v -> Int
sizeL Nil = 0
sizeL (Cons _ _ xs) = 1 + sizeL xs

singleton :: k -> v -> FullList k v
singleton k v = FL k v Nil

lookup :: Eq k => k -> FullList k v -> Maybe v
lookup !k (FL k' v xs)
    | k == k'   = Just v
    | otherwise = lookupL k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

lookupL :: Eq k => k -> List k v -> Maybe v
lookupL = go
  where
    go !_ Nil = Nothing
    go k (Cons k' v xs)
        | k == k'   = Just v
        | otherwise = go k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookupL #-}
#endif

member :: Eq k => k -> FullList k v -> Bool
member !k (FL k' _ xs)
    | k == k'   = True
    | otherwise = memberL k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE member #-}
#endif

memberL :: Eq k => k -> List k v -> Bool
memberL = go
  where
    go !_ Nil = False
    go k (Cons k' _ xs)
        | k == k'   = True
        | otherwise = go k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE memberL #-}
#endif

insert :: Eq k => k -> v -> FullList k v -> FullList k v
insert !k v (FL k' v' xs)
    | k == k'   = FL k v xs
    | otherwise = FL k' v' (insertL k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

-- | /O(n)/ Insert at the head of the list to avoid copying the whole
-- list.
insertL :: Eq k => k -> v -> List k v -> List k v
insertL = go
  where
    go !k v Nil = Cons k v Nil
    go k v (Cons k' v' xs)
        | k == k'   = Cons k v xs
        | otherwise = Cons k' v' (go k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertL #-}
#endif

delete :: Eq k => k -> FullList k v -> Maybe (FullList k v)
delete !k (FL k' v xs)
    | k == k'   = case xs of
        Nil             -> Nothing
        Cons k'' v' xs' -> Just $ FL k'' v' xs'
    | otherwise = let ys = deleteL k xs
                  in ys `seq` Just (FL k' v ys)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

deleteL :: Eq k => k -> List k v -> List k v
deleteL = go
  where
    go !_ Nil = Nil
    go k (Cons k' v xs)
        | k == k'   = xs
        | otherwise = Cons k' v (go k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deleteL #-}
#endif

insertWith :: Eq k => (v -> v -> v) -> k -> v -> FullList k v -> FullList k v
insertWith f !k v (FL k' v' xs)
    | k == k'   = FL k (f v v') xs
    | otherwise = FL k' v' (insertWithL f k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

insertWithL :: Eq k => (v -> v -> v) -> k -> v -> List k v -> List k v
insertWithL = go
  where
    go _ !k v Nil = Cons k v Nil
    go f k v (Cons k' v' xs)
        | k == k'   = Cons k (f v v') xs
        | otherwise = Cons k' v' (go f k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWithL #-}
#endif

adjust :: Eq k => (v -> v) -> k -> FullList k v -> FullList k v
adjust f !k (FL k' v xs)
  | k == k' = FL k' (f v) xs
  | otherwise = FL k' v (adjustL f k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjust #-}
#endif

adjustL :: Eq k => (v -> v) -> k -> List k v -> List k v
adjustL f = go
  where
    go !_ Nil = Nil
    go k (Cons k' v xs)
      | k == k' = Cons k' (f v) xs
      | otherwise = Cons k' v (go k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE adjustL #-}
#endif

------------------------------------------------------------------------
-- * Combine

-- | /O(n^2)/ Left biased union.
union :: Eq k => FullList k v -> FullList k v -> FullList k v
union xs (FL k v ys)
    | k `member` xs = unionL xs ys
    | otherwise     = case unionL xs ys of
        FL k' v' zs -> FL k v $ Cons k' v' zs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE union #-}
#endif

unionL :: Eq k => FullList k v -> List k v -> FullList k v
unionL xs@(FL k v zs) = FL k v . go
  where
    go Nil = zs
    go (Cons k' v' ys)
        | k' `member` xs = go ys
        | otherwise      = Cons k' v' $ go ys
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionL #-}
#endif

unionWith :: Eq k => (v -> v -> v) -> FullList k v -> FullList k v -> FullList k v
unionWith f xs (FL k vy ys) =
    case lookup k xs of
      Just vx ->
        let flCon = FL k (f vx vy)
        in case delete k xs of
          Nothing  -> flCon ys
          Just xs' ->
            case unionWithL f xs' ys of
              FL k' v' zs -> flCon $ Cons k' v' zs
      Nothing ->
        case unionWithL f xs ys of
          FL k' v' zs -> FL k vy $ Cons k' v' zs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionWith #-}
#endif

unionWithL :: Eq k => (v -> v -> v) -> FullList k v -> List k v -> FullList k v
unionWithL f (FL k v zs) ys =
  case lookupL k ys of
    Just vy -> FL k (f v vy) $ go zs (deleteL k ys)
    Nothing -> FL k v (go zs ys)
  where
    go ws Nil = ws
    go ws (Cons k' vy ys') =
      case lookupL k' ws of
        Just vx -> Cons k' (f vx vy) $ go (deleteL k' ws) ys'
        Nothing -> Cons k' vy $ go ws ys'
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE unionWithL #-}
#endif

------------------------------------------------------------------------
-- * Transformations

map :: (k1 -> v1 -> (k2, v2)) -> FullList k1 v1 -> FullList k2 v2
map f (FL k v xs) = let (k', v') = f k v
                    in FL k' v' (mapL f xs)
{-# INLINE map #-}

mapL :: (k1 -> v1 -> (k2, v2)) -> List k1 v1 -> List k2 v2
mapL f = go
  where
    go Nil = Nil
    go (Cons k v xs) = let (k', v') = f k v
                       in Cons k' v' (go xs)
{-# INLINE mapL #-}

traverseWithKey :: Applicative m => (k -> v1 -> m v2) -> FullList k v1 -> m (FullList k v2)
traverseWithKey f (FL k v xs) = FL k <$> f k v <*> traverseWithKeyL f xs
{-# INLINE traverseWithKey #-}

traverseWithKeyL :: Applicative m => (k -> v1 -> m v2) -> List k v1 -> m (List k v2)
traverseWithKeyL f = go
  where
    go Nil = pure Nil
    go (Cons k v xs) = Cons k <$> f k v <*> go xs
{-# INLINE traverseWithKeyL #-}

------------------------------------------------------------------------
-- * Folds

foldlWithKey' :: (a -> k -> v -> a) -> a -> FullList k v -> a
foldlWithKey' f !z (FL k v xs) = foldlWithKey'L f (f z k v) xs
{-# INLINE foldlWithKey' #-}

foldlWithKey'L :: (a -> k -> v -> a) -> a -> List k v -> a
foldlWithKey'L f = go
  where
    go !z Nil          = z
    go z (Cons k v xs) = go (f z k v) xs
{-# INLINE foldlWithKey'L #-}

foldrWithKey :: (k -> v -> a -> a) -> a -> FullList k v -> a
foldrWithKey f z (FL k v xs) = f k v (foldrWithKeyL f z xs)
{-# INLINE foldrWithKey #-}

foldrWithKeyL :: (k -> v -> a -> a) -> a -> List k v -> a
foldrWithKeyL f = go
  where
    go z Nil = z
    go z (Cons k v xs) = f k v (go z xs)
{-# INLINE foldrWithKeyL #-}

------------------------------------------------------------------------
-- * Filter

filterWithKey :: (k -> v -> Bool) -> FullList k v -> Maybe (FullList k v)
filterWithKey p (FL k v xs)
    | p k v     = Just (FL k v ys)
    | otherwise = case ys of
        Nil           -> Nothing
        Cons k' v' zs -> Just $ FL k' v' zs
  where !ys = filterWithKeyL p xs
{-# INLINE filterWithKey #-}

filterWithKeyL :: (k -> v -> Bool) -> List k v -> List k v
filterWithKeyL p = go
  where
    go Nil = Nil
    go (Cons k v xs)
        | p k v     = Cons k v (go xs)
        | otherwise = go xs
{-# INLINE filterWithKeyL #-}
