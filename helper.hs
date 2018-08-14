{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main
    ( module Main
    , module Data.HashMap.Base
    ) where

import Data.HashMap.Base
import Data.Hashable
import Test.QuickCheck

newtype Key = K
    { unK :: Int
    } deriving (Arbitrary, Eq, Ord, Read, Show)

instance Hashable Key where
    hashWithSalt salt k = Data.Hashable.hashWithSalt salt (unK k) `mod` 20

main = pure ()
