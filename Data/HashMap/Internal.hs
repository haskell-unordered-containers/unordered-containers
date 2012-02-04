{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

-- | Unsafe access to the constructors of a 'HashMap'.
module Data.HashMap.Internal
    ( HashMap(..)
    , SuffixMask, Hash
    , FullList(..), List(..)
    ) where

import Data.HashMap.Common
import Data.FullList.Lazy
