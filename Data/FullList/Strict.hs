{-# LANGUAGE BangPatterns #-}

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

import Data.FullList.Lazy
