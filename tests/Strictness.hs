{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Hashable (Hashable(hash))
import Test.ChasingBottoms.IsBottom
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(arbitrary))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
            deriving (Arbitrary, Eq, Ord, Show)

instance Hashable Key where
    hash k = hash (unK k) `mod` 20
    
instance (Arbitrary k, Arbitrary v, Eq k, Hashable k) =>
         Arbitrary (HashMap k v) where
    arbitrary = HM.fromList `fmap` arbitrary

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pInsertKeyStrict :: HashMap Key Int -> Int -> Bool
pInsertKeyStrict m v = isBottom $ HM.insert bottom v m

pInsertValueStrict :: HashMap Key Int -> Key -> Bool
pInsertValueStrict m k = isBottom $ HM.insert k bottom m

-- TODO: Add instance of 'CoArbitrary' that includes 'bottom'.

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
      testGroup "Strict"
      [ testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      ]
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests
