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
      return (keysToMap trues, keysToMap falses)
    setupInts s gen = do
      ints <- genInts s gen
      let (trues, falses) = Data.List.partition (flip testBit (31 :: Int)) ints
      return (keysToMap trues, keysToMap falses)

-- TODO: Separate benchmarks for overlap with pointer eq?!
bUnionOverlap :: [Benchmark]
bUnionOverlap =
  [ bgroup "Bytes" [env' s setupBytes run | s <- defaultSizes],
    bgroup "Int" [env' s setupInts run | s <- defaultSizes]
  ]
  where
    run :: (Hashable a) => (HashMap a Int, HashMap a Int) -> Benchmarkable
    run = whnf (\(as, bs) -> HM.union as bs)
    setupBytes s gen = do
      (trues, falses) <- Key.Bytes.genDisjoint s bytesLength gen
      let (a_sep, b_sep) = splitAt (s `div` 4) trues
      return
        ( keysToMap falses `HM.union` keysToMap a_sep,
          keysToMap falses `HM.union` keysToMap b_sep
        )
    setupInts s gen = do
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

bUnionEqual :: [Benchmark]
bUnionEqual =
  [ bgroup "Bytes" [env' s setupBytes run | s <- defaultSizes],
    bgroup "Int" [env' s setupInts run | s <- defaultSizes]
  ]
  where
    run :: (Hashable a) => HashMap a Int -> Benchmarkable
    run = whnf (\m -> HM.union m m)
    setupBytes s gen = do
      ks <- Key.Bytes.genNBytes s bytesLength gen
      return (keysToMap ks)
    setupInts s gen = do
      ks <- genInts s gen
      return (keysToMap ks)

bSetFromList :: Benchmark
bSetFromList =
  bgroup
    "fromList"
    [ bg "Bytes" setupBytes,
      bg "Int" setupInts
    ]
  where
    bg name e = bgroup name (b e)
    b e = [env' s e run | s <- defaultSizes]
    run :: (Hashable a) => [a] -> Benchmarkable
    run = whnf Data.HashSet.fromList
    setupBytes s gen = genNBytes s bytesLength gen
    setupInts = genInts

{-
bg :: _
bg name setup run = bgroup name (b setup run)
  where
    b e run = [env (e s) (run s) | s <- defaultSizes]
-}

keysToMap :: (Hashable k) => [k] -> HashMap k Int
keysToMap = HM.fromList . map (,1)

genInts ::
  (StatefulGen g m) =>
  Int ->
  g ->
  m [Int]
genInts n = replicateM n . uniformM

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
