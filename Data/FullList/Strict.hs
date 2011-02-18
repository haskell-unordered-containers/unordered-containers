{-# LANGUAGE BangPatterns, CPP #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.FullList.Strict
-- Copyright   :  2010-2011 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Non-empty lists of key/value pairs.  The lists are strict in the
-- keys and the values.

module Data.FullList.Strict
    ( FullList

      -- * Basic interface
    , size
    , singleton
    , lookup
    , insert
    , delete
    , insertWith

      -- * Transformations
    , map

      -- * Folds
    , foldlWithKey'
    , foldrWithKey

      -- * Filter
    , filterWithKey
    ) where

import Prelude hiding (lookup, map)

import Data.FullList.Lazy hiding (insertWith)

insertWith :: Eq k => (v -> v -> v) -> k -> v -> FullList k v -> FullList k v
insertWith f !k v (FL k' v' xs)
    | k == k'   = let v'' = f v v' in v'' `seq` FL k v'' xs
    | otherwise = FL k' v' (insertWithL f k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWith #-}
#endif

insertWithL :: Eq k => (v -> v -> v) -> k -> v -> List k v -> List k v
insertWithL = go
  where
    go _ !k v Nil = Cons k v Nil
    go f k v (Cons k' v' xs)
        | k == k'   = let v'' = f v v' in v'' `seq` Cons k v'' xs
        | otherwise = Cons k' v' (go f k v xs)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE insertWithL #-}
#endif
