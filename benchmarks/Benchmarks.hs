module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.Hashable (Hashable)
import qualified Data.HashMap as M
import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import Util

n :: Int
n = 2^(12 :: Int)

elems :: [(String, Int)]
elems = zip keys values

keys :: [String]
keys = rnd 8 n

values :: [Int]
values = [1..n]

benchmarks :: M.HashMap String Int -> Map.Map String Int -> [Benchmark]
benchmarks hm m =
    [ bgroup "lookup" 
      [ bench "HashMap" $ nf (lookup keys) hm
      , bench "Map" $ nf (lookupM keys) m
      ]
    , bgroup "insert" 
      [ bench "HashMap" $ nf (insert elems) M.empty
      , bench "Map" $ nf (insertM elems) Map.empty
      ]
    , bgroup "delete" 
      [ bench "HashMap" $ nf (delete keys) hm
      , bench "Map" $ nf (insertM elems) Map.empty
      ]
    ]

main :: IO ()
main = do
    let hm = fromList elems :: M.HashMap String Int
        m = Map.fromList elems :: Map.Map String Int
    defaultMainWith defaultConfig (liftIO . evaluate $ rnf [m])
        (benchmarks hm m)

------------------------------------------------------------------------
-- * HashMap

lookup :: [String] -> M.HashMap String Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (M.lookup k m)) 0 xs

insert :: [(String, Int)] -> M.HashMap String Int -> M.HashMap String Int
insert xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs

delete :: [String] -> M.HashMap String Int -> M.HashMap String Int
delete xs m0 = foldl' (\m k -> M.delete k m) m0 xs

------------------------------------------------------------------------
-- * Map

lookupM :: [String] -> Map.Map String Int -> Int
lookupM xs m = foldl' (\z k -> fromMaybe z (Map.lookup k m)) 0 xs

insertM :: [(String, Int)] -> Map.Map String Int -> Map.Map String Int
insertM xs m0 = foldl' (\m (k, v) -> Map.insert k v m) m0 xs

deleteM :: [String] -> Map.Map String Int -> Map.Map String Int
deleteM xs m0 = foldl' (\m k -> Map.delete k m) m0 xs

------------------------------------------------------------------------
-- * Helpers

fromList :: (Eq k, Hashable k) => [(k, v)] -> M.HashMap k v
fromList = foldl' (\m (k, v) -> M.insert k v m) M.empty
