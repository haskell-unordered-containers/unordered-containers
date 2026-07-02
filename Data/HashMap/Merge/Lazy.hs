{-# LANGUAGE Trustworthy #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Merge.Lazy
-- License     :  BSD-style
-- Maintainer  :  simon.jakobi@gmail.com, David.Feuer@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines an API for writing functions that merge two
-- maps. The key functions are 'merge' and (eventually) @mergeA@.
-- These have parametrized types that would be difficult to use
-- directly, but the modular structure means that most users will only
-- need a few simple pieces.
--
-- The 'merge' function is intended to be able to express /all/
-- (non-effectful) functions that combine two maps, such as 'Data.HashMap.Lazy.union',
-- 'Data.HashMap.Lazy.intersectionWith' and 'Data.HashMap.Lazy.difference':
--
-- @
-- 'Data.HashMap.Lazy.unionWith' f = 'merge' 'preserveMissing' 'preserveMissing' ('zipWithMatched' (const f))
-- 'Data.HashMap.Lazy.intersectionWith' f = 'merge' 'dropMissing' 'dropMissing' ('zipWithMatched' (const f))
-- 'Data.HashMap.Lazy.difference' = 'merge' 'preserveMissing' 'dropMissing' ('zipWithMaybeMatched' (\\_ _ _ -> Nothing))
-- @
--
-- This module is intended to mirror the API of "Data.Map.Merge.Lazy"
-- from @containers@.
--
-- @since 0.2.22.0
module Data.HashMap.Merge.Lazy
    (
      -- * Merging
      merge

      -- * Merge tactics
      -- ** When-missing tactics
    , SimpleWhenMissing
    , dropMissing
    , preserveMissing
    , mapMissing
    , mapMaybeMissing
    , filterMissing

      -- ** When-matched tactics
    , SimpleWhenMatched
    , zipWithMatched
    , zipWithMaybeMatched

      -- * General combining function types
    , WhenMissing
    , WhenMatched
    ) where

import Data.HashMap.Internal (SimpleWhenMatched, SimpleWhenMissing,
                              WhenMatched, WhenMissing, dropMissing,
                              filterMissing, mapMaybeMissing, mapMissing,
                              merge, preserveMissing, zipWithMatched,
                              zipWithMaybeMatched)
