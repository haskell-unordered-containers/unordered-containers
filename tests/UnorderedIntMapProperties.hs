{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

-- | Tests for the 'Data.UnorderedIntMap' module.  We test functions by
-- comparing them to a simpler model, an association list.

module Main (main) where

import Control.Monad ( guard )
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as L
import qualified Data.UnorderedIntMap as UIM
import qualified Data.IntMap as IM
import Test.QuickCheck (Arbitrary)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

type Key = Int

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Instances

pEq :: [(Key, Int)] -> [(Key, Int)] -> Bool
pEq xs = (IM.fromList xs ==) `eq` (UIM.fromList xs ==)

pNeq :: [(Key, Int)] -> [(Key, Int)] -> Bool
pNeq xs = (IM.fromList xs /=) `eq` (UIM.fromList xs /=)

-- We cannot compare to `Data.Map` as ordering is different.
pOrd1 :: [(Key, Int)] -> Bool
pOrd1 xs = compare x x == EQ
  where
    x = UIM.fromList xs

pOrd2 :: [(Key, Int)] -> [(Key, Int)] -> [(Key, Int)] -> Bool
pOrd2 xs ys zs = case (compare x y, compare y z) of
    (EQ, o)  -> compare x z == o
    (o,  EQ) -> compare x z == o
    (LT, LT) -> compare x z == LT
    (GT, GT) -> compare x z == GT
    (LT, GT) -> True -- ys greater than xs and zs.
    (GT, LT) -> True
  where
    x = UIM.fromList xs
    y = UIM.fromList ys
    z = UIM.fromList zs

pOrd3 :: [(Key, Int)] -> [(Key, Int)] -> Bool
pOrd3 xs ys = case (compare x y, compare y x) of
    (EQ, EQ) -> True
    (LT, GT) -> True
    (GT, LT) -> True
    _        -> False
  where
    x = UIM.fromList xs
    y = UIM.fromList ys

pOrdEq :: [(Key, Int)] -> [(Key, Int)] -> Bool
pOrdEq xs ys = case (compare x y, x == y) of
    (EQ, True)  -> True
    (LT, False) -> True
    (GT, False) -> True
    _           -> False
  where
    x = UIM.fromList xs
    y = UIM.fromList ys

pReadShow :: [(Key, Int)] -> Bool
pReadShow xs = IM.fromList xs == read (show (IM.fromList xs))

pFunctor :: [(Key, Int)] -> Bool
pFunctor = fmap (+ 1) `eq_` fmap (+ 1)

pFoldable :: [(Int, Int)] -> Bool
pFoldable = (L.sort . Foldable.foldr (:) []) `eq`
            (L.sort . Foldable.foldr (:) [])

------------------------------------------------------------------------
-- ** Basic interface

pSize :: [(Key, Int)] -> Bool
pSize = IM.size `eq` UIM.size

pMember :: Key -> [(Key, Int)] -> Bool
pMember k = IM.member k `eq` UIM.member k

pLookup :: Key -> [(Key, Int)] -> Bool
pLookup k = IM.lookup k `eq` UIM.lookup k

pInsert :: Key -> Int -> [(Key, Int)] -> Bool
pInsert k v = IM.insert k v `eq_` UIM.insert k v

pDelete :: Key -> [(Key, Int)] -> Bool
pDelete k = IM.delete k `eq_` UIM.delete k

newtype AlwaysCollide = AC Int
    deriving (Arbitrary, Eq, Ord, Show)

pInsertWith :: Key -> [(Key, Int)] -> Bool
pInsertWith k = IM.insertWith (+) k 1 `eq_` UIM.insertWith (+) k 1

pAdjust :: Key -> [(Key, Int)] -> Bool
pAdjust k = IM.adjust succ k `eq_` UIM.adjust succ k

pUpdateAdjust :: Key -> [(Key, Int)] -> Bool
pUpdateAdjust k = IM.update (Just . succ) k `eq_` UIM.update (Just . succ) k

pUpdateDelete :: Key -> [(Key, Int)] -> Bool
pUpdateDelete k = IM.update (const Nothing) k `eq_` UIM.update (const Nothing) k

pAlterAdjust :: Key -> [(Key, Int)] -> Bool
pAlterAdjust k = IM.alter (fmap succ) k `eq_` UIM.alter (fmap succ) k

pAlterInsert :: Key -> [(Key, Int)] -> Bool
pAlterInsert k = IM.alter (const $ Just 3) k `eq_` UIM.alter (const $ Just 3) k

pAlterDelete :: Key -> [(Key, Int)] -> Bool
pAlterDelete k = IM.alter (const Nothing) k `eq_` UIM.alter (const Nothing) k

------------------------------------------------------------------------
-- ** Combine

pUnion :: [(Key, Int)] -> [(Key, Int)] -> Bool
pUnion xs ys = IM.union (IM.fromList xs) `eq_` UIM.union (UIM.fromList xs) $ ys

pUnionWith :: [(Key, Int)] -> [(Key, Int)] -> Bool
pUnionWith xs ys = IM.unionWith (-) (IM.fromList xs) `eq_`
                   UIM.unionWith (-) (UIM.fromList xs) $ ys

pUnionWithKey :: [(Key, Int)] -> [(Key, Int)] -> Bool
pUnionWithKey xs ys = IM.unionWithKey go (IM.fromList xs) `eq_`
                             UIM.unionWithKey go (UIM.fromList xs) $ ys
  where
    go :: Key -> Int -> Int -> Int
    go k i1 i2 = k - i1 + i2

pUnions :: [[(Key, Int)]] -> Bool
pUnions xss = IM.toAscList (IM.unions (map IM.fromList xss)) ==
              toAscList (UIM.unions (map UIM.fromList xss))

------------------------------------------------------------------------
-- ** Transformations

pMap :: [(Key, Int)] -> Bool
pMap = IM.map (+ 1) `eq_` UIM.map (+ 1)

------------------------------------------------------------------------
-- ** Difference and intersection

pDifference :: [(Key, Int)] -> [(Key, Int)] -> Bool
pDifference xs ys = IM.difference (IM.fromList xs) `eq_`
                    UIM.difference (UIM.fromList xs) $ ys

pDifferenceWith :: [(Key, Int)] -> [(Key, Int)] -> Bool
pDifferenceWith xs ys = IM.differenceWith f (IM.fromList xs) `eq_`
                        UIM.differenceWith f (UIM.fromList xs) $ ys
  where
    f x y = if x == 0 then Nothing else Just (x - y)

pIntersection :: [(Key, Int)] -> [(Key, Int)] -> Bool
pIntersection xs ys = IM.intersection (IM.fromList xs) `eq_`
                      UIM.intersection (UIM.fromList xs) $ ys

pIntersectionWith :: [(Key, Int)] -> [(Key, Int)] -> Bool
pIntersectionWith xs ys = IM.intersectionWith (-) (IM.fromList xs) `eq_`
                          UIM.intersectionWith (-) (UIM.fromList xs) $ ys

pIntersectionWithKey :: [(Key, Int)] -> [(Key, Int)] -> Bool
pIntersectionWithKey xs ys = IM.intersectionWithKey go (IM.fromList xs) `eq_`
                             UIM.intersectionWithKey go (UIM.fromList xs) $ ys
  where
    go :: Key -> Int -> Int -> Int
    go k i1 i2 = k - i1 - i2

------------------------------------------------------------------------
-- ** Folds

pFoldr :: [(Int, Int)] -> Bool
pFoldr = (L.sort . IM.fold (:) []) `eq` (L.sort . UIM.foldr (:) [])

pFoldrWithKey :: [(Int, Int)] -> Bool
pFoldrWithKey = (sortByKey . IM.foldrWithKey f []) `eq`
                (sortByKey . UIM.foldrWithKey f [])
  where f k v z = (k, v) : z

pFoldl' :: Int -> [(Int, Int)] -> Bool
pFoldl' z0 = foldlWithKey'Map (\ z _ v -> v + z) z0 `eq` UIM.foldl' (+) z0

foldlWithKey'Map :: (b -> Int -> a -> b) -> b -> IM.IntMap a -> b
#if MIN_VERSION_containers(4,2,0)
foldlWithKey'Map = IM.foldlWithKey'
#else
-- Equivalent except for bottoms, which we don't test.
foldlWithKey'Map = IM.foldlWithKey
#endif

------------------------------------------------------------------------
-- ** Filter

pMapMaybeWithKey :: [(Key, Int)] -> Bool
pMapMaybeWithKey = IM.mapMaybeWithKey f `eq_` UIM.mapMaybeWithKey f
  where f k v = guard (odd (k + v)) >> Just (v + 1)

pMapMaybe :: [(Key, Int)] -> Bool
pMapMaybe = IM.mapMaybe f `eq_` UIM.mapMaybe f
  where f v = guard (odd v) >> Just (v + 1)

pFilter :: [(Key, Int)] -> Bool
pFilter = IM.filter odd `eq_` UIM.filter odd

pFilterWithKey :: [(Key, Int)] -> Bool
pFilterWithKey = IM.filterWithKey p `eq_` UIM.filterWithKey p
  where p k v = odd (k + v)

------------------------------------------------------------------------
-- ** Conversions

-- 'eq_' already calls fromList.
pFromList :: [(Key, Int)] -> Bool
pFromList = id `eq_` id

pFromListWith :: [(Key, Int)] -> Bool
pFromListWith kvs = (IM.toAscList $ IM.fromListWith (+) kvs) ==
                    (toAscList $ UIM.fromListWith (+) kvs)

pToList :: [(Key, Int)] -> Bool
pToList = IM.toAscList `eq` toAscList

pElems :: [(Key, Int)] -> Bool
pElems = (L.sort . IM.elems) `eq` (L.sort . UIM.elems)

pKeys :: [(Key, Int)] -> Bool
pKeys = (L.sort . IM.keys) `eq` (L.sort . UIM.keys)

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Instances
      testGroup "instances"
      [ testProperty "==" pEq
      , testProperty "/=" pNeq
      , testProperty "compare reflexive" pOrd1
      , testProperty "compare transitive" pOrd2
      , testProperty "compare antisymmetric" pOrd3
      , testProperty "Ord => Eq" pOrdEq
      , testProperty "Read/Show" pReadShow
      , testProperty "Functor" pFunctor
      , testProperty "Foldable" pFoldable
      ]
    -- Basic interface
    , testGroup "basic interface"
      [ testProperty "size" pSize
      , testProperty "member" pMember
      , testProperty "lookup" pLookup
      , testProperty "insert" pInsert
      , testProperty "delete" pDelete
      , testProperty "insertWith" pInsertWith
      , testProperty "adjust" pAdjust
      , testProperty "updateAdjust" pUpdateAdjust
      , testProperty "updateDelete" pUpdateDelete
      , testProperty "alterAdjust" pAlterAdjust
      , testProperty "alterInsert" pAlterInsert
      , testProperty "alterDelete" pAlterDelete
      ]
    -- Combine
    , testProperty "union" pUnion
    , testProperty "unionWith" pUnionWith
    , testProperty "unionWithKey" pUnionWithKey
    , testProperty "unions" pUnions
    -- Transformations
    , testProperty "map" pMap
    -- Folds
    , testGroup "folds"
      [ testProperty "foldr" pFoldr
      , testProperty "foldrWithKey" pFoldrWithKey
      , testProperty "foldl'" pFoldl'
      ]
    , testGroup "difference and intersection"
      [ testProperty "difference" pDifference
      , testProperty "differenceWith" pDifferenceWith
      , testProperty "intersection" pIntersection
      , testProperty "intersectionWith" pIntersectionWith
      , testProperty "intersectionWithKey" pIntersectionWithKey
      ]
    -- Filter
    , testGroup "filter"
      [ testProperty "filter" pFilter
      , testProperty "filterWithKey" pFilterWithKey
      , testProperty "mapMaybe" pMapMaybe
      , testProperty "mapMaybeWithKey" pMapMaybeWithKey
      ]
    -- Conversions
    , testGroup "conversions"
      [ testProperty "elems" pElems
      , testProperty "keys" pKeys
      , testProperty "fromList" pFromList
      , testProperty "fromListWith" pFromListWith
      , testProperty "toList" pToList
      ]
    ]

------------------------------------------------------------------------
-- * Model

type Model v = IM.IntMap v

-- | Check that a function operating on a 'HashMap' is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a)
   => (Model v -> a)       -- ^ Function that modifies a 'Model'
   -> (UIM.UnorderedIntMap v -> a)  -- ^ Function that modified a 'HashMap' in the same
                             -- way
   -> [(Int, v)]               -- ^ Initial content of the 'HashMap' and 'Model'
   -> Bool                   -- ^ True if the functions are equivalent
eq f g xs = g (UIM.fromList xs) == f (IM.fromList xs)

eq_ :: (Eq v)
    => (Model v -> Model v)            -- ^ Function that modifies a 'Model'
    -> (UIM.UnorderedIntMap v -> UIM.UnorderedIntMap v)  -- ^ Function that modified a
                                           -- 'HashMap' in the same way
    -> [(Int, v)]                            -- ^ Initial content of the 'HashMap'
                                           -- and 'Model'
    -> Bool                                -- ^ True if the functions are
                                           -- equivalent
eq_ f g = (IM.toAscList . f) `eq` (toAscList . g)

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Helpers

sortByKey :: [(Int, v)] -> [(Int, v)]
sortByKey = L.sortBy (compare `on` fst)

toAscList :: UIM.UnorderedIntMap v -> [(Int, v)]
toAscList = L.sortBy (compare `on` fst) . UIM.toList