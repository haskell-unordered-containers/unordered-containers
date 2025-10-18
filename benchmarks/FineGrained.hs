{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

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
          -- bgroup "insert" bInsert
          bUnion
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

env' ::
  (NFData a) =>
  Int ->
  (Int -> IOGenM StdGen -> IO a) ->
  (a -> Benchmarkable) ->
  Benchmark
env' size setup run =
  env
    ( do
        gen <- newIOGenM defaultGen
        setup size gen
    )
    (\x -> bench (show size) (run x))

bFromList :: Benchmark
bFromList =
  bgroup
    "fromList"
    [ bgroup "Bytes" [env' s setupBytes run | s <- defaultSizes],
      bgroup "Int" [env' s genInts run | s <- defaultSizes]
    ]
  where
    setupBytes s = genNBytes s bytesLength
    run :: (Hashable a) => [a] -> Benchmarkable
    run = whnf (HM.fromList . map (,()))

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
  [ bgroup "Bytes" [env' s setupBytes run | s <- defaultSizes],
    bgroup "Int" [env' s setupInts run | s <- defaultSizes]
  ]
  where
    run :: (Hashable a) => (HashMap a Int, HashMap a Int) -> Benchmarkable
    run = whnf (\(as, bs) -> HM.union as bs)
    setupBytes s gen = do
      (trues, falses) <- Key.Bytes.genDisjoint s bytesLength gen
      return (toMap trues, toMap falses)
    setupInts s gen = do
      ints <- genInts s gen
      let (trues, falses) = Data.List.partition (flip testBit (31 :: Int)) ints
      return (toMap trues, toMap falses)

bUnionEqual :: [Benchmark]
bUnionEqual =
  [ bgroup "Bytes" [env (bytesEnv s) (bench' s) | s <- defaultSizes],
    bgroup "Int" [env (intsEnv s) (bench' s) | s <- defaultSizes]
  ]
  where
    bench' s = bench (show s) . whnf (\m -> HM.union m m)
    bytesEnv s = do
      g <- newIOGenM defaultGen
      ks <- Key.Bytes.genNBytes s bytesLength g
      return (toMap ks)
    intsEnv s = do
      g <- newIOGenM defaultGen
      ks <- genInts s g
      return (toMap ks)

-- TODO: Separate benchmarks for overlap with pointer eq?!
bUnionOverlap :: [Benchmark]
bUnionOverlap =
  [ bgroup "Bytes" [env (bytesEnv s) (bench' s) | s <- defaultSizes],
    bgroup "Int" [env (intsEnv s) (bench' s) | s <- defaultSizes]
  ]
  where
    bench' s tup = bench (show s) $ whnf (\(as, bs) -> HM.union as bs) tup
    bytesEnv s = do
      g <- newIOGenM defaultGen
      (trues, falses) <- Key.Bytes.genDisjoint s bytesLength g
      let (a_sep, b_sep) = splitAt (s `div` 4) trues
      return (toMap falses `HM.union` toMap a_sep, toMap falses `HM.union` toMap b_sep)
    intsEnv s = do
      g <- newIOGenM defaultGen
      let s_overlap = s `div` 2
      let s_a_sep = (s - s_overlap) `div` 2
      let s_b_sep = s - s_overlap - s_a_sep
      overlap <- genInts s_overlap g
      a_sep <- genInts s_a_sep g
      b_sep <- genInts s_b_sep g
      return (toMap overlap `HM.union` toMap a_sep, toMap overlap `HM.union` toMap b_sep)

toMap :: (Hashable k) => [k] -> HashMap k Int
toMap = HM.fromList . map (,1)

genInts ::
  (StatefulGen g m) =>
  Int ->
  g ->
  m [Int]
genInts n = do
  replicateM n . uniformM

bSetFromList :: Benchmark
bSetFromList =
  bgroup
    "fromList"
    [ bgroup "Bytes" (b bytesEnv),
      bgroup "Int" (b intsEnv)
    ]
  where
    b e = [env (e s) (bench' s) | s <- defaultSizes]
    bench' s = bench (show s) . whnf Data.HashSet.fromList
    bytesEnv s = do
      g <- newIOGenM defaultGen
      genNBytes s bytesLength g
    intsEnv s = do
      g <- newIOGenM defaultGen
      genInts s g

{-
bFromList = matrix defaultSizes e' b'
  where
    e' s = uniformListM s defaultGen
    b' = whnf HM.fromList
-}

{-
bInsert = [ env m $ \d -> bench (show s) $ whnf (\(k, v, m) -> HM.insert k v m) d ]
  where m s = do
          g <- newIOGenM defaultGen
          let hm = HM.empty
          forM_ [1..s] $ \v -> do
            b <- genBytes 32 g
            HMI.unsafeInsert b v hm
          return (m, newKeys) -- separate existing, new
-}

{-
matrix :: (NFData env) => [Int] -> (Int -> IO env) -> (env -> Benchmarkable) -> Benchmark
matrix sizes e x = b -- [ b @Bytes, b @Int] -- , b @SlowInt, b @Colli ]
  where
    b = bgroup "bla" [runTemplate @Int e x s | s <- sizes]

runTemplate :: forall env. (NFData env) => (Int -> IO env) -> (env -> Benchmarkable) -> Int -> Benchmark
runTemplate e b s = env (e s) $ \x -> bench (show s) (b x)
-}
