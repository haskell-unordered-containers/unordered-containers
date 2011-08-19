module Data.HashMap.Lazy
    (
      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , HM.null
    , size
    , HM.lookup
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
    , HM.map
    , traverseWithKey

      -- * Difference and intersection
    , difference
    , intersection

      -- * Folds
    , foldl'
    , foldlWithKey'
    , HM.foldr
    , foldrWithKey

      -- * Filter
    , HM.filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    ) where

import Data.HashMap.Base as HM
