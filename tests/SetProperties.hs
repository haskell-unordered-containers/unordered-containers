 {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Tests for the 'Data.HashSet' module.  We test functions by
-- comparing them to a simpler model, a list.

module Main (main) where

import qualified Data.Foldable as Foldable
import Data.Hashable (Hashable(hash))
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.Set as Set
import Test.QuickCheck (Arbitrary)
import Test.Framework (Test, defaultMain, testGroup)
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

pEq :: [Key] -> [Key] -> Bool
pEq xs = (unique xs ==) `eq` (S.fromList xs ==)

pNeq :: [Key] -> [Key] -> Bool
pNeq xs = (unique xs /=) `eq` (S.fromList xs /=)

pFoldable :: [Int] -> Bool
pFoldable = (L.sort . Foldable.foldr (:) []) `eq`
            (L.sort . Foldable.foldr (:) [])

------------------------------------------------------------------------
-- ** Basic interface

pSize :: [Key] -> Bool
pSize = length `eq` S.size

pMember :: Key -> [Key] -> Bool
pMember k = L.elem k `eq` S.member k

pInsert :: Key -> [Key] -> Bool
pInsert a = insert a `eq` (toAscList . S.insert a)

pDelete :: Key -> [Key] -> Bool
pDelete a = delete a `eq` (toAscList . S.delete a)

------------------------------------------------------------------------
-- ** Combine

pUnion :: [Key] -> [Key] -> Bool
pUnion xs ys = L.sort (L.union as bs) ==
               toAscList (S.union (S.fromList as) (S.fromList bs))
  where
    as = fromList xs
    bs = fromList ys

------------------------------------------------------------------------
-- ** Transformations

pMap :: [Key] -> Bool
pMap = map f `eq` (toAscList . S.map f)
  where f (K k) = K (k + 1)

------------------------------------------------------------------------
-- ** Folds

pFoldr :: [Int] -> Bool
pFoldr = (L.sort . L.foldr (:) []) `eq`
         (L.sort . S.foldr (:) [])

pFoldl' :: Int -> [Int] -> Bool
pFoldl' z0 = L.foldl' (+) z0 `eq` S.foldl' (+) z0

------------------------------------------------------------------------
-- ** Conversions

pToList :: [Key] -> Bool
pToList = id `eq` toAscList

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Instances
      testGroup "instances"
      [ testProperty "==" pEq
      , testProperty "/=" pNeq
      , testProperty "Foldable" pFoldable
      ]
    -- Basic interface
    , testGroup "basic interface"
      [ testProperty "size" pSize
      , testProperty "member" pMember
      , testProperty "insert" pInsert
      , testProperty "delete" pDelete
      ]
    -- Combine
    , testProperty "union" pUnion
    -- Transformations
    , testProperty "map" pMap
    -- Folds
    , testGroup "folds"
      [ testProperty "foldr" pFoldr
      , testProperty "foldl'" pFoldl'
      ]
    -- Conversions
    , testGroup "conversions"
      [ testProperty "toList" pToList
      ]
    ]

------------------------------------------------------------------------
-- * Model

-- Invariant: the list is sorted in ascending order, by key.
type Model a = [a]

-- | Check that a function operating on a 'HashMap' is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a, Hashable a, Ord a, Eq b)
   => (Model a -> b)      -- ^ Function that modifies a 'Model' in the same
                          -- way
   -> (S.HashSet a -> b)  -- ^ Function that modified a 'HashSet'
   -> [a]                 -- ^ Initial content of the 'HashSet' and 'Model'
   -> Bool                -- ^ True if the functions are equivalent
eq f g xs = g (S.fromList ys) == f ys
  where ys = fromList xs

insert :: Ord a => a -> Model a -> Model a
insert x [] = [x]
insert x (y:xs)
    | x == y    = x : xs
    | x > y     = y : insert x xs
    | otherwise = x : y : xs

delete :: Ord a => a -> Model a -> Model a
delete _ [] = []
delete k ys@(y:xs)
    | k == y   = xs
    | k > y    = y : delete k xs
    | otherwise = ys

-- | Create a model from a list of key-value pairs.  If the input
-- contains multiple entries for the same key, the latter one is used.
fromList :: Ord a => [a] -> Model a
fromList = L.foldl' (\ m p -> insert p m) []

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Helpers

toAscList :: Ord a => S.HashSet a -> [a]
toAscList = L.sort . S.toList

unique :: (Eq a, Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList
