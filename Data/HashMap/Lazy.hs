module Data.HashMap.Lazy
    (
      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , lookup
    , lookupDefault
    , insert
    , insertWith
    , delete
    , adjust

      -- * Combine
      -- * Union
    , union
    , unionWith

      -- * Transformations
    , map
    , traverseWithKey

      -- * Difference and intersection
    , difference
    , intersection

      -- * Folds
    , foldl'
    , foldlWithKey'
    , foldr
    , foldrWithKey

      -- * Filter
    , filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    ) where

import Prelude hiding (filter, foldr, lookup, map, null)

import Data.HashMap.Base
