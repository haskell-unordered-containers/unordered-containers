module Data.HashMap
    ( HashMap
    , empty
    , singleton
    , null
    , insert
    , lookup
    , fromList
    , toList
    ) where

import Control.Monad.ST (runST)
import Data.Hashable (Hashable)
import Prelude hiding (lookup, null)

import Data.HashMap.Base
import qualified Data.HashMap.Mutable as M

-- | /O(n)/ Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList xs = runST (M.freeze =<< M.fromList xs)
{-# INLINABLE fromList #-}
