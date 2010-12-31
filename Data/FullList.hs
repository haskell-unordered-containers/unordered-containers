{-# LANGUAGE BangPatterns, CPP #-}

module Data.FullList
    ( FullList

      -- * Basic interface
    , size
    , singleton
    , lookup
    , insert
    , delete
    , fold
    ) where

import Control.DeepSeq (NFData(rnf))
import Prelude hiding (lookup)

------------------------------------------------------------------------
-- * The 'FullList' type

data FullList k v = FL !k !v !(List k v)
                  deriving Show

instance (NFData k, NFData v) => NFData (FullList k v)

data List k v = Nil | Cons !k !v !(List k v)
              deriving Show

instance (NFData k, NFData v) => NFData (List k v) where
    rnf Nil           = ()
    rnf (Cons k v xs) = rnf k `seq` rnf v `seq` rnf xs

-- TODO: Check if evaluation is forced.

------------------------------------------------------------------------
-- * FullList

size :: FullList k v -> Int
size (FL _ _ xs) = 1 + sizeL xs

singleton :: k -> v -> FullList k v
singleton k v = FL k v Nil

lookup :: Eq k => k -> FullList k v -> Maybe v
lookup !k (FL k' v xs)
    | k == k'   = Just v
    | otherwise = lookupL k xs
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE lookup #-}
#endif

insert :: Eq k => k -> v -> FullList k v -> FullList k v
insert !k v (FL k' v' xs)
    | k == k'   = FL k v xs
    | otherwise = FL k' v' (insertL k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insert #-}
#endif

delete :: Eq k => k -> FullList k v -> Maybe (FullList k v)
delete !k (FL k' v xs)
    | k == k'   = case xs of
        Nil             -> Nothing
        Cons k'' v' xs' -> Just $ FL k'' v' xs'
    | otherwise = Just $ FL k' v (deleteL k xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE delete #-}
#endif

fold :: (k -> v -> a -> a) -> a -> FullList k v -> a
fold f z (FL k v xs) = f k v (foldL f z xs)
{-# INLINE fold #-}
    
------------------------------------------------------------------------
-- * List

sizeL :: List k v -> Int
sizeL Nil = 0
sizeL (Cons _ _ xs) = 1 + sizeL xs

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

-- TODO: Reduce copying by always inserting at the head of the list?

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

foldL :: (k -> v -> a -> a) -> a -> List k v -> a
foldL f = go
  where
    go z Nil = z
    go z (Cons k v xs) = f k v (go z xs)
{-# INLINE foldL #-}
