{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Tests for the 'Data.HashMap.Lazy' module.  We test functions by
-- comparing them to a simpler model, an association list.

module Main (main) where

import Data.Function (on)
import Data.Hashable (Hashable(hash))
import qualified Data.List as L
import qualified Data.HashMap.Lazy as M
import Test.QuickCheck (Arbitrary)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
            deriving (Arbitrary, Eq, Ord, Show)

instance Hashable Key where
    hash k = hash (unK k) `mod` 20

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Instances

pEq :: [(Key, Int)] -> [(Key, Int)] -> Bool
pEq xs = (xs ==) `eq` (M.fromList xs ==)

pNeq :: [(Key, Int)] -> [(Key, Int)] -> Bool
pNeq xs = (xs /=) `eq` (M.fromList xs /=)

------------------------------------------------------------------------
-- ** Basic interface

pSize :: [(Key, Int)] -> Bool
pSize = length `eq` M.size

pLookup :: Key -> [(Key, Int)] -> Bool
pLookup k = L.lookup k `eq` M.lookup k

pInsert :: Key -> Int -> [(Key, Int)] -> Bool
pInsert k v = insert (k, v) `eq` (toAscList . M.insert k v)

pDelete :: Key -> [(Key, Int)] -> Bool
pDelete k = delete k `eq` (toAscList . M.delete k)

pInsertWith :: Key -> [(Key, Int)] -> Bool
pInsertWith k = insertWith (+) (k, 1) `eq`
                       (toAscList . M.insertWith (+) k 1)

pToList :: [(Key, Int)] -> Bool
pToList = id `eq` toAscList

tests :: [Test]
tests =
    [ testProperty "==" pEq
    , testProperty "/=" pNeq
    , testProperty "size" pSize
    , testProperty "lookup" pLookup
    , testProperty "insert" pInsert
    , testProperty "delete" pDelete
    , testProperty "insertWith" pInsertWith

      -- Transformations
    , testProperty "map" pMap

      -- Folds
    , testProperty "foldr" pFoldr
    , testProperty "foldl'" pFoldl'

      -- Conversions
    , testProperty "toList" pToList
    ]

------------------------------------------------------------------------
-- ** Transformations

pMap :: [(Key, Int)] -> Bool
pMap = map (\ (k, v) -> (k, v + 1)) `eq` (toAscList . M.map (+ 1))

------------------------------------------------------------------------
-- ** Folds

pFoldr :: [(Int, Int)] -> Bool
pFoldr = (sortByKey . L.foldr (:) []) `eq`
         (sortByKey . M.foldrWithKey f [])
  where f k v z = (k, v) : z

pFoldl' :: Int -> [(Int, Int)] -> Bool
pFoldl' z0 = L.foldl' (\ z (_, v) -> z + v) z0 `eq` M.foldl' (+) z0

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
eq f g xs = g (M.fromList ys) == f ys
  where ys = fromList xs

insert :: Ord k => (k, v) -> Model k v -> Model k v
insert x [] = [x]
insert x@(k, _) (y@(k', _):xs)
    | k == k'   = x : xs
    | k > k'    = y : insert x xs
    | otherwise = x : y : xs

delete :: Ord k => k -> Model k v -> Model k v
delete _ [] = []
delete k ys@(y@(k', _):xs)
    | k == k'   = xs
    | k > k'    = y : delete k xs
    | otherwise = ys

insertWith :: Ord k => (v -> v -> v) -> (k, v) -> Model k v -> Model k v
insertWith _ x [] = [x]
insertWith f x@(k, v) (y@(k', v'):xs)
    | k == k'   = (k', f v v') : xs
    | k > k'    = y : insertWith f x xs
    | otherwise = x : y : xs

-- | Create a model from a list of key-value pairs.  If the input
-- contains multiple entries for the same key, the latter one is used.
fromList :: Ord k => [(k, v)] -> Model k v
fromList = L.foldl' ( \ m p -> insert p m) []

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- Helpers

sortByKey :: Ord k => [(k, v)] -> [(k, v)]
sortByKey = L.sortBy (compare `on` fst)

toAscList :: Ord k => M.HashMap k v -> [(k, v)]
toAscList = sortByKey . M.toList
