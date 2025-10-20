{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Bifunctor (second)
import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
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
          bInsert,
          bUnion,
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

-- 100 insertions each, so we get more precise timings
bInsert :: Benchmark
bInsert =
  bgroup
    "insert"
    [ bgroup "presentKey" bInsertPresentKey,
      bgroup "absentKey" bInsertAbsentKey
    ]

bInsertPresentKey :: [Benchmark]
bInsertPresentKey =
  [ bgroup "sameValue" bInsertPresentKeySameValue
  , bgroup "differentValue" bInsertPresentKeyDifferentValue
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
    toKVs = take 100 . Data.List.cycle . map (second (+1)) . HM.toList
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
