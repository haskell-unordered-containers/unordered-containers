{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import qualified Data.HashMap.PopCount as PC

main :: IO ()
main = defaultMain
       [ bench "popCount" $ whnf popCount 100000
       ]

popCount :: Int -> Int
popCount n = go n 0
  where
    go !n !acc | n > 0 = go (n-1) (PC.popCount $ fromIntegral n)
               | otherwise = acc
