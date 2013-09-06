{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Hashable (Hashable(hashWithSalt))
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
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 20

instance (Arbitrary k, Arbitrary v, Eq k, Hashable k) =>
         Arbitrary (HashMap k v) where
    arbitrary = HM.fromList `fmap` arbitrary

instance Show (Int -> Int) where
    show _ = "<function>"

instance Show (Int -> Int -> Int) where
    show _ = "<function>"

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ HM.singleton (bottom :: Key) v

pSingletonValueStrict :: Key -> Bool
pSingletonValueStrict k = isBottom $ (HM.singleton k (bottom :: Int))

pLookupDefaultKeyStrict :: Int -> HashMap Key Int -> Bool
pLookupDefaultKeyStrict def m = isBottom $ HM.lookupDefault def bottom m

pAdjustKeyStrict :: (Int -> Int) -> HashMap Key Int -> Bool
pAdjustKeyStrict f m = isBottom $ HM.adjust f bottom m

pAdjustValueStrict :: Key -> HashMap Key Int -> Bool
pAdjustValueStrict k m
    | k `HM.member` m = isBottom $ HM.adjust (const bottom) k m
    | otherwise       = case HM.keys m of
        []     -> True
        (k':_) -> isBottom $ HM.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> HashMap Key Int -> Bool
pInsertKeyStrict v m = isBottom $ HM.insert bottom v m

pInsertValueStrict :: Key -> HashMap Key Int -> Bool
pInsertValueStrict k m = isBottom $ HM.insert k bottom m

pInsertWithKeyStrict :: (Int -> Int -> Int) -> Int -> HashMap Key Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ HM.insertWith f bottom v m

pInsertWithValueStrict :: (Int -> Int -> Int) -> Key -> Int -> HashMap Key Int
                       -> Bool
pInsertWithValueStrict f k v m
    | HM.member k m = isBottom $ HM.insertWith (const2 bottom) k v m
    | otherwise     = isBottom $ HM.insertWith f k bottom m

pFromListKeyStrict :: Bool
pFromListKeyStrict = isBottom $ HM.fromList [(undefined :: Key, 1 :: Int)]

pFromListValueStrict :: Bool
pFromListValueStrict = isBottom $ HM.fromList [(K 1, undefined)]

pFromListWithKeyStrict :: (Int -> Int -> Int) -> Bool
pFromListWithKeyStrict f =
    isBottom $ HM.fromListWith f [(undefined :: Key, 1 :: Int)]

pFromListWithValueStrict :: [(Key, Int)] -> Bool
pFromListWithValueStrict xs = case xs of
    [] -> True
    (x:_) -> isBottom $ HM.fromListWith (\ _ _ -> undefined) (x:xs)

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Basic interface
      testGroup "HashMap.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict HM.member
      , testProperty "lookup is key-strict" $ keyStrict HM.lookup
      , testProperty "lookupDefault is key-strict" pLookupDefaultKeyStrict
      , testProperty "! is key-strict" $ keyStrict (flip (HM.!))
      , testProperty "delete is key-strict" $ keyStrict HM.delete
      , testProperty "adjust is key-strict" pAdjustKeyStrict
      , testProperty "adjust is value-strict" pAdjustValueStrict
      , testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      , testProperty "insertWith is key-strict" pInsertWithKeyStrict
      , testProperty "insertWith is value-strict" pInsertWithValueStrict
      , testProperty "fromList is key-strict" pFromListKeyStrict
      , testProperty "fromList is value-strict" pFromListValueStrict
      , testProperty "fromListWith is key-strict" pFromListWithKeyStrict
      , testProperty "fromListWith is value-strict" pFromListWithValueStrict
      ]
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Key -> HashMap Key Int -> a) -> HashMap Key Int -> Bool
keyStrict f m = isBottom $ f bottom m

const2 :: a -> b -> c -> a
const2 x _ _ = x
