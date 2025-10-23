{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This file is formatted with https://hackage.haskell.org/package/ormolu
module Main where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Data.Bifunctor (second)
import Data.Bits (testBit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet
import Data.Hashable
import Data.List
import Key.Bytes
import System.Random.Stateful
import Test.Tasty.Bench
import Prelude hiding (Foldable (..), lookup)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "HashMap.Strict"
        [ bFromList,
          bLookup,
          bInsert,
          bDelete,
          bUnion,
          bUnions,
          bIntersection,
          bDifference
        ],
      bgroup "HashSet" [bSetFromList]
    ]

defaultSizes :: [Int]
defaultSizes = [0, 1, 10, 100, 1000, 10_000, 100_000]

-- | Length of a 'Bytes' key in bytes.
--
-- For comparison: A SHA256 hash is 32 bytes long.
bytesLength :: Int
bytesLength = 32

-- | Pseudo-random generator for keys etc.
--
-- Change the seed to generate different random elements.
defaultGen :: StdGen
defaultGen = mkStdGen 42

bFromList :: Benchmark
bFromList =
  bgroup
    "fromList"
    [ bgroup' "Bytes" setupBytes b,
      bgroup' "Int" genInts b
    ]
  where
    setupBytes s gen = genNBytes s bytesLength gen
    b s = bench (show s) . whnf (HM.fromList . map (,()))

-- 100 lookups each, so we get more precise timings
bLookup :: Benchmark
bLookup =
  bgroup
    "lookup"
    [ bgroup "presentKey" bLookupPresentKey,
      bgroup "absentKey" bLookupAbsentKey
    ]

bLookupPresentKey :: [Benchmark]
bLookupPresentKey =
  [ bgroup'WithSizes sizes "Bytes" setupBytes b,
    bgroup'WithSizes sizes "Int" setupInts b
  ]
  where
    sizes = filter (/= 0) defaultSizes
    b s =
      bench (show s)
        . whnf (\(m, ks) -> foldl' (\() k -> HM.lookup k m `seq` ()) () ks)
    toKs = take 1000 . Data.List.cycle . HM.keys
    setupBytes size gen = do
      m <- genBytesMap size gen
      return (m, toKs m)
    setupInts size gen = do
      m <- genIntMap size gen
      return (m, toKs m)

bLookupAbsentKey :: [Benchmark]
bLookupAbsentKey =
  [ bgroup' "Bytes" setupBytes b,
    bgroup' "Int" setupInts b
  ]
  where
    b s =
      bench (show s)
        . whnf (\(m, ks) -> foldl' (\() k -> HM.lookup k m `seq` ()) () ks)
    setupBytes size gen = do
      m <- genBytesMap size gen
      ks0 <- genNBytes 1100 bytesLength gen
      let ks1 = take 1000 $ Data.List.cycle $ filter (not . flip HM.member m) ks0
      return (m, ks1)
    setupInts size gen = do
      m <- genIntMap size gen
      ks0 <- genInts 1100 gen
      let ks1 = take 1000 $ Data.List.cycle $ filter (not . flip HM.member m) ks0
      return (m, ks1)

-- 100 insertions each, so we get more precise timings
bInsert :: Benchmark
bInsert =
  bgroup
    "insert"
    [ bgroup
        "presentKey"
        [ bgroup "sameValue" bInsertPresentKeySameValue,
          bgroup "differentValue" bInsertPresentKeyDifferentValue
        ],
      bgroup "absentKey" bInsertAbsentKey
    ]

bInsertPresentKeySameValue :: [Benchmark]
bInsertPresentKeySameValue =
  [ bgroup'WithSizes sizes "Bytes" setupBytes b,
    bgroup'WithSizes sizes "Int" setupInts b
  ]
  where
    sizes = filter (/= 0) defaultSizes
    b s =
      bench (show s)
        . whnf (\(m, kvs) -> foldl' (\() (k, v) -> HM.insert k v m `seq` ()) () kvs)
    toKVs = take 100 . Data.List.cycle . HM.toList
    setupBytes size gen = do
      m <- genBytesMap size gen
      return (m, toKVs m)
    setupInts size gen = do
      m <- genIntMap size gen
      return (m, toKVs m)

bInsertPresentKeyDifferentValue :: [Benchmark]
bInsertPresentKeyDifferentValue =
  [ bgroup'WithSizes sizes "Bytes" setupBytes b,
    bgroup'WithSizes sizes "Int" setupInts b
  ]
  where
    sizes = filter (/= 0) defaultSizes
    b s =
      bench (show s)
        . whnf (\(m, kvs) -> foldl' (\() (k, v) -> HM.insert k v m `seq` ()) () kvs)
    toKVs = take 100 . Data.List.cycle . map (second (+ 1)) . HM.toList
    setupBytes size gen = do
      m <- genBytesMap size gen
      return (m, toKVs m)
    setupInts size gen = do
      m <- genIntMap size gen
      return (m, toKVs m)

bInsertAbsentKey :: [Benchmark]
bInsertAbsentKey =
  [ bgroup' "Bytes" setupBytes b,
    bgroup' "Int" setupInts b
  ]
  where
    b s =
      bench (show s)
        . whnf (\(m, kvs) -> foldl' (\() (k, v) -> HM.insert k v m `seq` ()) () kvs)
    setupBytes size gen = do
      m <- genBytesMap size gen
      ks <- genNBytes 200 bytesLength gen
      let kvs = take 100 $ Data.List.cycle $ map (,1) $ filter (not . flip HM.member m) ks
      return (m, kvs)
    setupInts size gen = do
      m <- genIntMap size gen
      ks <- genInts 200 gen
      let kvs = take 100 $ Data.List.cycle $ map (,1) $ filter (not . flip HM.member m) ks
      return (m, kvs)

-- 100 deletions each, so we get more precise timings
bDelete :: Benchmark
bDelete =
  bgroup
    "delete"
    [ bgroup "presentKey" bDeletePresentKey,
      bgroup "absentKey" bDeleteAbsentKey
    ]

bDeletePresentKey :: [Benchmark]
bDeletePresentKey =
  [ bgroup'WithSizes sizes "Bytes" setupBytes b,
    bgroup'WithSizes sizes "Int" setupInts b
  ]
  where
    sizes = filter (/= 0) defaultSizes
    b s =
      bench (show s)
        . whnf (\(m, ks) -> foldl' (\() k -> HM.delete k m `seq` ()) () ks)
    toKs = take 100 . Data.List.cycle . HM.keys
    setupBytes size gen = do
      m <- genBytesMap size gen
      return (m, toKs m)
    setupInts size gen = do
      m <- genIntMap size gen
      return (m, toKs m)

bDeleteAbsentKey :: [Benchmark]
bDeleteAbsentKey =
  [ bgroup' "Bytes" setupBytes b,
    bgroup' "Int" setupInts b
  ]
  where
    b s =
      bench (show s)
        . whnf (\(m, ks) -> foldl' (\() k -> HM.delete k m `seq` ()) () ks)
    setupBytes size gen = do
      m <- genBytesMap size gen
      ks0 <- genNBytes 200 bytesLength gen
      let ks1 = take 100 $ Data.List.cycle $ filter (not . flip HM.member m) ks0
      return (m, ks1)
    setupInts size gen = do
      m <- genIntMap size gen
      ks0 <- genInts 200 gen
      let ks1 = take 100 $ Data.List.cycle $ filter (not . flip HM.member m) ks0
      return (m, ks1)

-- TODO: For the "overlap" and "equal" cases, it would be interesting to
-- have separate benchmarks both with and without shared subtrees,
-- so we can make use of pointer equality.
bUnion :: Benchmark
bUnion =
  bgroup
    "union"
    [ bgroup "disjoint" bUnionDisjoint,
      bgroup "overlap" bUnionOverlap,
      bgroup "equal" bUnionEqual
    ]

bUnionDisjoint :: [Benchmark]
bUnionDisjoint =
  [ bgroup' "Bytes" genBytesMapsDisjoint b,
    bgroup' "Int" genIntMapsDisjoint b
  ]
  where
    b s = bench (show s) . whnf (\(as, bs) -> HM.union as bs)

bUnionOverlap :: [Benchmark]
bUnionOverlap =
  [ bgroup' "Bytes" genBytesMapsOverlap b,
    bgroup' "Int" genIntMapsOverlap b
  ]
  where
    b s = bench (show s) . whnf (\(as, bs) -> HM.union as bs)

bUnionEqual :: [Benchmark]
bUnionEqual =
  [ bgroup' "Bytes" genBytesMap b,
    bgroup' "Int" genIntMap b
  ]
  where
    b size = bench (show size) . whnf (\m -> HM.union m m)

bUnions :: Benchmark
bUnions =
  bgroup
    "unions"
    [ bgroup'WithSizes sizes "Bytes" setupBytes b,
      bgroup'WithSizes sizes "Int" setupInts b
    ]
  where
    sizes = filter (>= 10) defaultSizes
    b size = bench (show size) . whnf (\ms -> HM.unions ms)
    setupBytes s gen = replicateM 10 (genBytesMap (s `div` 10) gen)
    setupInts s gen = replicateM 10 (genBytesMap (s `div` 10) gen)

-- TODO: For the "overlap" and "equal" cases, it would be interesting to
-- have separate benchmarks both with and without shared subtrees,
-- so we can make use of pointer equality.
bIntersection :: Benchmark
bIntersection =
  bgroup
    "intersection"
    [ bgroup "disjoint" bIntersectionDisjoint,
      bgroup "overlap" bIntersectionOverlap,
      bgroup "equal" bIntersectionEqual
    ]

bIntersectionDisjoint :: [Benchmark]
bIntersectionDisjoint =
  [ bgroup' "Bytes" genBytesMapsDisjoint b,
    bgroup' "Int" genIntMapsDisjoint b
  ]
  where
    b size = bench (show size) . whnf (\(xs, ys) -> HM.intersection xs ys)

bIntersectionOverlap :: [Benchmark]
bIntersectionOverlap =
  [ bgroup' "Bytes" genBytesMapsOverlap b,
    bgroup' "Int" genIntMapsOverlap b
  ]
  where
    b size = bench (show size) . whnf (\(xs, ys) -> HM.intersection xs ys)

bIntersectionEqual :: [Benchmark]
bIntersectionEqual =
  [ bgroup' "Bytes" genBytesMap b,
    bgroup' "Int" genIntMap b
  ]
  where
    b size = bench (show size) . whnf (\m -> HM.intersection m m)

-- TODO: For the "overlap" and "equal" cases, it would be interesting to
-- have separate benchmarks both with and without shared subtrees,
-- so we can make use of pointer equality.
bDifference :: Benchmark
bDifference =
  bgroup
    "difference"
    [ bgroup "disjoint" bDifferenceDisjoint,
      bgroup "overlap" bDifferenceOverlap,
      bgroup "equal" bDifferenceEqual
    ]

bDifferenceDisjoint :: [Benchmark]
bDifferenceDisjoint =
  [ bgroup' "Bytes" genBytesMapsDisjoint b,
    bgroup' "Int" genIntMapsDisjoint b
  ]
  where
    b size = bench (show size) . whnf (\(xs, ys) -> HM.difference xs ys)

bDifferenceOverlap :: [Benchmark]
bDifferenceOverlap =
  [ bgroup' "Bytes" genBytesMapsOverlap b,
    bgroup' "Int" genIntMapsOverlap b
  ]
  where
    b size = bench (show size) . whnf (\(xs, ys) -> HM.difference xs ys)

bDifferenceEqual :: [Benchmark]
bDifferenceEqual =
  [ bgroup' "Bytes" genBytesMap b,
    bgroup' "Int" genIntMap b
  ]
  where
    b size = bench (show size) . whnf (\m -> HM.difference m m)

bSetFromList :: Benchmark
bSetFromList =
  bgroup
    "fromList"
    [ bgroup' "Bytes" (\s gen -> genNBytes s bytesLength gen) b,
      bgroup' "Int" genInts b
    ]
  where
    b size = bench (show size) . whnf Data.HashSet.fromList

-------------------------------------------------------------------------------
-- Boilerplate

bgroup' ::
  (NFData env) =>
  String ->
  (Int -> IOGenM StdGen -> IO env) ->
  (Int -> env -> Benchmark) ->
  Benchmark
bgroup' = bgroup'WithSizes defaultSizes

bgroup'WithSizes ::
  (NFData env) =>
  [Int] ->
  String ->
  (Int -> IOGenM StdGen -> IO env) ->
  (Int -> env -> Benchmark) ->
  Benchmark
bgroup'WithSizes sizes name setup b = bgroup name [env' setup b s | s <- sizes]

env' ::
  (NFData env) =>
  (Int -> IOGenM StdGen -> IO env) ->
  (Int -> env -> Benchmark) ->
  Int ->
  Benchmark
env' setup b size =
  env
    ( do
        gen <- newIOGenM defaultGen
        setup size gen
    )
    (b size)

-------------------------------------------------------------------------------
-- Generators

keysToMap :: (Hashable k) => [k] -> HashMap k Int
keysToMap = HM.fromList . map (,1)

genInts ::
  (StatefulGen g m) =>
  Int ->
  g ->
  m [Int]
genInts n = replicateM n . uniformM

genBytesMap :: (StatefulGen g m) => Int -> g -> m (HashMap Bytes Int)
genBytesMap s gen = do
  ks <- Key.Bytes.genNBytes s bytesLength gen
  return (keysToMap ks)

genIntMap :: (StatefulGen g m) => Int -> g -> m (HashMap Int Int)
genIntMap s gen = do
  ks <- genInts s gen
  return (keysToMap ks)

genBytesMapsOverlap ::
  (StatefulGen g m) =>
  Int -> g -> m (HashMap Bytes Int, HashMap Bytes Int)
genBytesMapsOverlap s gen = do
  (trues, falses) <- Key.Bytes.genDisjoint s bytesLength gen
  let (a_sep, b_sep) = splitAt (s `div` 4) trues
  return
    ( keysToMap falses `HM.union` keysToMap a_sep,
      keysToMap falses `HM.union` keysToMap b_sep
    )

genIntMapsOverlap ::
  (StatefulGen g m) =>
  Int -> g -> m (HashMap Int Int, HashMap Int Int)
genIntMapsOverlap s gen = do
  let s_overlap = s `div` 2
  let s_a_sep = (s - s_overlap) `div` 2
  let s_b_sep = s - s_overlap - s_a_sep
  overlap <- genInts s_overlap gen
  a_sep <- genInts s_a_sep gen
  b_sep <- genInts s_b_sep gen
  return
    ( keysToMap overlap `HM.union` keysToMap a_sep,
      keysToMap overlap `HM.union` keysToMap b_sep
    )

genIntMapsDisjoint ::
  (StatefulGen g m) =>
  Int -> g -> m (HashMap Int Int, HashMap Int Int)
genIntMapsDisjoint s gen = do
  ints <- genInts s gen
  let (trues, falses) = Data.List.partition (flip testBit (31 :: Int)) ints
  return (keysToMap trues, keysToMap falses)

genBytesMapsDisjoint ::
  (StatefulGen g m) =>
  Int -> g -> m (HashMap Bytes Int, HashMap Bytes Int)
genBytesMapsDisjoint s gen = do
  (trues, falses) <- Key.Bytes.genDisjoint s bytesLength gen
  return (keysToMap trues, keysToMap falses)
