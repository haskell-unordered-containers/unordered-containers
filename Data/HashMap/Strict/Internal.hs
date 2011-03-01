{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Strict.Internal
-- Copyright   :  2010-2011 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Semi-public internals.

module Data.HashMap.Strict.Internal
    ( collisions
    , collisionHistogram
    ) where

import Data.HashMap.Lazy.Internal
