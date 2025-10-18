{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (replicateM)
import Data.Bits (testBit)
import qualified Data.HashMap.Strict as HM
import Data.List
import Key.Bytes
import System.Random.Stateful
import Test.Tasty.Bench
import Prelude hiding (Foldable (..), lookup)

main :: IO ()
main =
  defaultMain
    [ bFromList,
      -- bgroup "insert" bInsert
      bUnion
    ]

defaultGen :: StdGen
defaultGen = mkStdGen 42

defaultSizes :: [Int]
defaultSizes = [0, 1, 10, 100, 1000, 10_000, 100_000]

bytesLength :: Int
bytesLength = 32

bFromList :: Benchmark
bFromList =
  bgroup
    "fromList"
    [ bgroup
        "Bytes"
        [ env
            ( do
                g <- newIOGenM defaultGen
                genNBytes s bytesLength g
            )
            $ \keys ->
              bench (show s) $ whnf (HM.fromList . map (,())) keys
        | s <- sizes
        ],
      bgroup
        "Int"
        [ env
            ( do
                g <- newIOGenM defaultGen
                genInts s g
            )
            $ \keys -> bench (show s) $ whnf (HM.fromList . map (,())) keys
        | s <- sizes
        ]
    ]
  where
    sizes = defaultSizes

bUnion :: Benchmark
bUnion =
  bgroup
    "union"
    [bgroup "disjoint" bUnionDisjoint, bgroup "overlap" bUnionOverlap, bgroup "same" []]

bUnionDisjoint :: [Benchmark]
bUnionDisjoint =
  [ bgroup "Bytes" [env (bytesEnv s) (bench' s) | s <- defaultSizes],
    bgroup "Int" [env (intsEnv s) (bench' s) | s <- defaultSizes]
  ]
  where
    bench' s tup = bench (show s) $ whnf (\(as, bs) -> HM.union as bs) tup
    bytesEnv s = do
      g <- newIOGenM defaultGen
      (trues, falses) <- Key.Bytes.genDisjoint s bytesLength g
      return (HM.fromList (map (,()) trues), HM.fromList (map (,()) falses))
    intsEnv s = do
      g <- newIOGenM defaultGen
      ints <- genInts s g
      let (trues, falses) = Data.List.partition (flip testBit (31 :: Int)) ints
      return (HM.fromList (map (,()) trues), HM.fromList (map (,()) falses))

bUnionOverlap :: [Benchmark]
bUnionOverlap = undefined

genInts ::
  (StatefulGen g m) =>
  Int ->
  g ->
  m [Int]
genInts n = do
  replicateM n . uniformM

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
