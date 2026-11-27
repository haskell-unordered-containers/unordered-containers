{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
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
--
-- = Description
--
-- Utilities for working in 'ST'.
module Data.HashMap.Internal.ST
  ( foldlST'
  ) where

import Control.Monad.ST
-- for MicroHS
import Data.Foldable (Foldable (foldr))
#ifdef __GLASGOW_HASKELL__
import GHC.Exts (oneShot)
#endif
import Prelude hiding (foldr)

-- | A strict version of 'Data.Foldable.foldlM', specialized for 'ST'. The problem
-- with just using @foldlM@ is that GHC doesn't always get the arity right, which
-- can lead to excessive allocation. See
-- https://github.com/haskell-unordered-containers/unordered-containers/pull/602#issuecomment-6775859682
-- and https://gitlab.haskell.org/ghc/ghc/-/issues/16357.
-- We don't actually need the strictness for @fromList@ and such, but we include
-- it to reduce the number of things we have to check.
foldlST' :: Foldable f => (b -> a -> ST s b) -> b -> f a -> ST s b
foldlST' f z0 xs = foldr (\x k -> oneShot (\ !z -> f z x >>= k)) pure xs z0
{-# INLINE foldlST' #-}

#ifndef __GLASGOW_HASKELL__
oneShot :: a -> a
oneShot f = f
#endif
