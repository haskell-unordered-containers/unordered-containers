{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

-- | Unsafe access to the constructor of a 'HashSet'.
module Data.HashSet.Internal ( HashSet(..) ) where

import Data.HashMap.Common (HashMap)

-- | A set of values.  A set cannot contain duplicate values.
newtype HashSet a = HashSet {
      asMap :: HashMap a ()
    }
