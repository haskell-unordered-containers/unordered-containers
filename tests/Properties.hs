{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Tests for the 'Data.HashMap' module.  We test functions by
-- comparing them to a simpler model, an association list.

module Main (main) where

import Data.Function (on)
import Data.Hashable (Hashable(hash))
import qualified Data.List as L
import qualified Data.HashMap as M
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Batch

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
            deriving (Arbitrary, Eq, Ord, Show)

instance Hashable Key where
    hash k = hash (unK k) `mod` 20

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Basic interface

pLookup :: Key -> [(Key, Int)] -> Bool
pLookup k = L.lookup k `eq` M.lookup k

pInsert :: Key -> Int -> [(Key, Int)] -> Bool
pInsert k v = insert (k, v) `eq` (toAscList . M.insert k v)

pSingleton :: Key -> Int -> Bool
pSingleton k v = singleton k v == M.toList (M.singleton k v)

tests :: [TestOptions -> IO TestResult]
tests =
    [ run pLookup
    , run pInsert
    , run pSingleton
    ]

------------------------------------------------------------------------
-- Model

-- Invariant: the list is sorted in ascending order, by key.
type Model k v = [(k, v)]

-- | Check that a function operating on a 'HashMap' is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a, Eq k, Hashable k, Ord k)
   => (Model k v -> a)      -- ^ Function that modifies a 'Model' in the same
                            -- way
   -> (M.HashMap k v -> a)  -- ^ Function that modified a 'HashMap'
   -> [(k, v)]              -- ^ Initial content of the 'HashMap' and 'Model'
   -> Bool                  -- ^ True if the functions are equivalent
eq f g xs = g (fromList ys) == f ys
  where ys = L.nubBy ((==) `on` fst) $ L.sortBy (compare `on` fst) $ xs

insert :: Ord k => (k, v) -> Model k v -> Model k v
insert x [] = [x]
insert x@(k, _) (y@(k', _):xs)
    | k == k'   = x : xs
    | k > k'    = y : insert x xs
    | otherwise = x : y : xs

singleton :: k -> v -> Model k v
singleton k v = [(k,v)]

delete :: Ord k => k -> Model k v -> Model k v
delete _ [] = []
delete k ys@(y@(k', _):xs)
    | k == k'   = xs
    | k > k'    = y : delete k xs
    | otherwise = ys

------------------------------------------------------------------------
-- Test harness

options :: TestOptions
options = TestOptions
    { no_of_tests     = 500
    , length_of_tests = 1
    , debug_tests     = False
    }

main :: IO ()
main = runTests "basics" options tests

------------------------------------------------------------------------
-- Helpers

fromList :: (Eq k, Hashable k) => [(k, v)] -> M.HashMap k v
fromList = L.foldl' ins M.empty
  where ins m (k, v) = M.insert k v m

sortByKey :: Ord k => [(k, v)] -> [(k, v)]
sortByKey = L.sortBy (compare `on` fst)

toAscList :: Ord k => M.HashMap k v -> [(k, v)]
toAscList = sortByKey . M.toList
