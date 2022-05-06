{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- because of the Arbitrary instances

-- | Tests for the 'Data.HashSet' module.  We test functions by
-- comparing them to @Set@ from @containers@.

module Properties.HashSet (tests) where

import Data.Hashable         (Hashable (hashWithSalt))
import Data.Ord              (comparing)
import Test.QuickCheck       (Property, property, (===), (==>))
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), testProperty)
import Util.Key              (Key, incKey, keyToInt)
import Data.HashSet (HashSet)
import Data.HashMap.Lazy (HashMap)
import Data.Set (Set)

import qualified Data.Foldable as Foldable
import qualified Data.HashSet  as HS
import qualified Data.List     as List
import qualified Data.Set      as Set
import qualified Data.Set      as S
import qualified Data.HashMap.Lazy as HM

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary
  shrink = fmap HM.fromList . shrink . HM.toList

instance (Eq a, Hashable a, Arbitrary a) => Arbitrary (HashSet a) where
  arbitrary = HS.fromMap <$> arbitrary
  shrink = fmap HS.fromMap . shrink . HS.toMap

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Instances



------------------------------------------------------------------------
-- ** Basic interface

pSize :: [Key] -> Property
pSize = Set.size `eq` HS.size

pMember :: Key -> [Key] -> Property
pMember k = Set.member k `eq` HS.member k

pInsert :: Key -> [Key] -> Property
pInsert a = Set.insert a `eq_` HS.insert a

pDelete :: Key -> [Key] -> Property
pDelete a = Set.delete a `eq_` HS.delete a

------------------------------------------------------------------------
-- ** Combine

pUnion :: [Key] -> [Key] -> Property
pUnion xs ys = Set.union (Set.fromList xs) `eq_`
               HS.union (HS.fromList xs) $ ys

------------------------------------------------------------------------
-- ** Transformations

pMap :: [Key] -> Property
pMap = Set.map incKey `eq_` HS.map incKey

------------------------------------------------------------------------
-- ** Folds

pFoldr :: [Int] -> Property
pFoldr = (List.sort . foldrSet (:) []) `eq`
         (List.sort . HS.foldr (:) [])

foldrSet :: (a -> b -> b) -> b -> Set.Set a -> b
foldrSet = Set.foldr

pFoldl' :: Int -> [Int] -> Property
pFoldl' z0 = foldl'Set (+) z0 `eq` HS.foldl' (+) z0

foldl'Set :: (a -> b -> a) -> a -> Set.Set b -> a
foldl'Set = Set.foldl'

------------------------------------------------------------------------
-- ** Filter

pFilter :: [Key] -> Property
pFilter = Set.filter p `eq_` HS.filter p
  where
    p = odd . keyToInt

------------------------------------------------------------------------
-- ** Conversions

pToList :: [Key] -> Property
pToList = Set.toAscList `eq` toAscList

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests = testGroup "Data.HashSet"
  [ -- Instances
    testGroup "instances"
    [ testGroup "Eq"
      [ testProperty "==" $
        \(x :: HSK) y -> (x == y) === (toOrdSet x == toOrdSet y)
      , testProperty "== permutations" $
        \(xs :: [Key]) (is :: [Int]) ->
          let shuffle idxs = List.map snd
                           . List.sortBy (comparing fst)
                           . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
              ys = shuffle is xs
          in  HS.fromList xs === HS.fromList ys
      , testProperty "/=" $
        \(x :: HSK) y -> (x /= y) === (toOrdSet x /= toOrdSet y)
      ]
    , testGroup "Ord"
      [ testProperty "compare reflexive" $
        -- We cannot compare to `Data.Map` as ordering is different.
        \(x :: HSK) -> compare x x === EQ
      , testProperty "compare transitive" $
        \(x :: HSK) y z -> case (compare x y, compare y z) of
          (EQ, o)  -> compare x z === o
          (o,  EQ) -> compare x z === o
          (LT, LT) -> compare x z === LT
          (GT, GT) -> compare x z === GT
          (LT, GT) -> property True -- ys greater than xs and zs.
          (GT, LT) -> property True
      , testProperty "compare antisymmetric" $
        \(x :: HSK) y -> case (compare x y, compare y x) of
          (EQ, EQ) -> True
          (LT, GT) -> True
          (GT, LT) -> True
          _        -> False
      , testProperty "Ord => Eq" $
        \(x :: HSK) y -> case (compare x y, x == y) of
          (EQ, True)  -> True
          (LT, False) -> True
          (GT, False) -> True
          _           -> False
      ]
    , testProperty "Read/Show" $
      \(x :: HSK) -> x === read (show x)
    , testProperty "Foldable" $
      \(x :: HSK) ->
        List.sort (Foldable.foldr (:) [] x)
        ===
        List.sort (Foldable.foldr (:) [] (toOrdSet x))
    , testProperty "Hashable" $
      \(xs :: [Key]) (is :: [Int]) salt ->
        let shuffle idxs = List.map snd
                 . List.sortBy (comparing fst)
                 . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
            xs' = List.nub xs
            ys = shuffle is xs'
            x = HS.fromList xs'
            y = HS.fromList ys
        in  x == y ==> hashWithSalt salt x === hashWithSalt salt y
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
  -- Filter
  , testGroup "filter"
    [ testProperty "filter" pFilter
    ]
  -- Conversions
  , testGroup "conversions"
    [ testProperty "toList" pToList
    ]
  ]

------------------------------------------------------------------------
-- * Model

-- Invariant: the list is sorted in ascending order, by key.
type Model a = Set.Set a

-- | Check that a function operating on a 'HashMap' is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a, Hashable a, Ord a, Show a, Eq b, Show b)
   => (Model a -> b)      -- ^ Function that modifies a 'Model' in the same
                          -- way
   -> (HS.HashSet a -> b)  -- ^ Function that modified a 'HashSet'
   -> [a]                 -- ^ Initial content of the 'HashSet' and 'Model'
   -> Property
eq f g xs = f (Set.fromList xs) === g (HS.fromList xs)

eq_ :: (Eq a, Hashable a, Ord a, Show a)
    => (Model a -> Model a)          -- ^ Function that modifies a 'Model'
    -> (HS.HashSet a -> HS.HashSet a)  -- ^ Function that modified a
                                     -- 'HashSet' in the same way
    -> [a]                           -- ^ Initial content of the 'HashSet'
                                     -- and 'Model'
    -> Property
eq_ f g = (Set.toAscList . f) `eq` (toAscList . g)

------------------------------------------------------------------------
-- * Helpers

type HSK = HashSet Key

toAscList :: Ord a => HS.HashSet a -> [a]
toAscList = List.sort . HS.toList

toOrdSet :: Ord a => HashSet a -> Set a
toOrdSet = S.fromList . HS.toList
