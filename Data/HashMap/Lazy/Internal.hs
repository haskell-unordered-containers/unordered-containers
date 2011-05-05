{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Lazy.Internal
-- Copyright   :  2010-2011 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Semi-public internals.

module Data.HashMap.Lazy.Internal
    ( collisions
    , collisionHistogram
    ) where

import Prelude hiding (lookup)

import qualified Data.FullList.Lazy as FL
import Data.HashMap.Common (HashMap(..))
import Data.HashMap.Lazy (insert, lookup)

-------------------------------------------------------------------
-- Metadata about map behavior

-- | /O(n)/ Return the number of hash collisions in this map.
collisions :: HashMap k v -> Int
collisions t = go t 0
  where
    go (Bin _ l r) !sz = go r (go l sz)
    go (Tip _ l)     !sz
      | fl_sz <= 1 = sz
      | otherwise  = sz + fl_sz
      where fl_sz = FL.size l
    go Nil           !sz = sz

-- | /O(n)/ Return histogram of hash collisions in this map.
-- Keys are number of entries in bucket, values are number of buckets
-- of that size.
collisionHistogram :: HashMap k v -> HashMap Int Int
collisionHistogram t = go t Nil
  where
    go (Bin _ l r) h = go r (go l h)
    go (Tip _ l)     h = (insert sz $! maybe 1 (1+) (lookup sz h)) h
      where sz = FL.size l
    go Nil           h = h
