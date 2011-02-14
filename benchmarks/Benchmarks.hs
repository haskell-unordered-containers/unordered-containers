{-# LANGUAGE GADTs #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.Hashable (Hashable)
import qualified Data.ByteString as BS
import qualified Data.HashMap as HM
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import qualified Util.ByteString as UBS
import qualified Util.Int as UI
import qualified Util.String as US

instance NFData BS.ByteString

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let hm   = HM.fromList elems :: HM.HashMap String Int
        hmbs = HM.fromList elemsBS :: HM.HashMap BS.ByteString Int
        hmi  = HM.fromList elemsI :: HM.HashMap Int Int
        m    = M.fromList elems :: M.Map String Int
        mbs  = M.fromList elemsBS :: M.Map BS.ByteString Int
        im   = IM.fromList elemsI :: IM.IntMap Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [B m, B mbs, B hm, B hmbs, B hmi, B im])
        [
          -- * Comparison to other data structures
          -- ** Map
          bgroup "Map"
          [ bgroup "lookup"
            [ bench "String" $ whnf (lookupM keys) m
            , bench "ByteString" $ whnf (lookupM keysBS) mbs
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insertM elems) M.empty
            , bench "ByteStringString" $ whnf (insertM elemsBS) M.empty
            ]
          , bgroup "updateDef"
            [ bench "String" $ whnf (updateDefM elems) M.empty
            , bench "ByteStringString" $ whnf (updateDefM elemsBS) M.empty
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (insertM elems) M.empty
            , bench "ByteString" $ whnf (insertM elemsBS) M.empty
            ]
          ]

          -- ** IntMap
        , bgroup "IntMap"
          [ bench "lookup" $ whnf (lookupIM keysI) im
          , bench "insert" $ whnf (insertIM elemsI) IM.empty
          , bench "updateDef" $ whnf (updateDefIM elemsI) IM.empty
          , bench "delete" $ whnf (deleteIM keysI) im
          ]

          -- * Basic interface
        , bgroup "lookup"
          [ bench "String" $ whnf (lookup keys) hm
          , bench "ByteString" $ whnf (lookup keysBS) hmbs
          , bench "Int" $ whnf (lookup keysI) hmi
          ]
        , bgroup "insert"
          [ bench "String" $ whnf (insert elems) HM.empty
          , bench "ByteString" $ whnf (insert elemsBS) HM.empty
          , bench "Int" $ whnf (insert elemsI) HM.empty
          ]
        , bgroup "updateDef"
          [ bench "String" $ whnf (updateWithDefault elems) HM.empty
          , bench "ByteString" $ whnf (updateWithDefault elemsBS) HM.empty
          , bench "Int" $ whnf (updateWithDefault elemsI) HM.empty
          ]
        , bgroup "fromList"
          [ bench "pure" $ whnf fromList elemsI
          , bench "mutating" $ whnf HM.fromList elemsI
          ]
        ]
  where
    n :: Int
    n = 2^(12 :: Int)

    elems   = zip keys [1..n]
    keys    = US.rnd 8 n
    elemsBS = zip keysBS [1..n]
    keysBS  = UBS.rnd 8 n
    elemsI  = zip keysI [1..n]
    keysI   = UI.rnd n n

------------------------------------------------------------------------
-- * HashMap

lookup :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (HM.lookup k m)) 0 xs
{-# SPECIALIZE lookup :: [Int] -> HM.HashMap Int Int -> Int #-}
{-# SPECIALIZE lookup :: [String] -> HM.HashMap String Int -> Int #-}
{-# SPECIALIZE lookup :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> Int #-}

insert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
       -> HM.HashMap k Int
insert xs m0 = foldl' (\m (k, v) -> HM.insert k v m) m0 xs
{-# SPECIALIZE insert :: [(Int, Int)] -> HM.HashMap Int Int
                      -> HM.HashMap Int Int #-}
{-# SPECIALIZE insert :: [(String, Int)] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE insert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

updateWithDefault :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
                  -> HM.HashMap k Int
updateWithDefault xs m0 =
    foldl' (\m (k, v) -> HM.updateWithDefault (+) k v m) m0 xs
{-# SPECIALIZE updateWithDefault :: [(Int, Int)] -> HM.HashMap Int Int
                                 -> HM.HashMap Int Int #-}
{-# SPECIALIZE updateWithDefault :: [(String, Int)] -> HM.HashMap String Int
                                 -> HM.HashMap String Int #-}
{-# SPECIALIZE updateWithDefault :: [(BS.ByteString, Int)]
                                 -> HM.HashMap BS.ByteString Int
                                 -> HM.HashMap BS.ByteString Int #-}

fromList :: [(Int, Int)] -> HM.HashMap Int Int
fromList xs = foldl' (\ m (k, v) -> HM.insert k v m) HM.empty xs

------------------------------------------------------------------------
-- * Map

lookupM :: Ord k => [k] -> M.Map k Int -> Int
lookupM xs m = foldl' (\z k -> fromMaybe z (M.lookup k m)) 0 xs
{-# SPECIALIZE lookupM :: [String] -> M.Map String Int -> Int #-}
{-# SPECIALIZE lookupM :: [BS.ByteString] -> M.Map BS.ByteString Int -> Int #-}

insertM :: Ord k => [(k, Int)] -> M.Map k Int -> M.Map k Int
insertM xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs
{-# SPECIALIZE insertM :: [(String, Int)] -> M.Map String Int
                       -> M.Map String Int #-}
{-# SPECIALIZE insertM :: [(BS.ByteString, Int)] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

updateDefM :: Ord k => [(k, Int)] -> M.Map k Int -> M.Map k Int
updateDefM xs m0 = foldl' (\m (k, v) -> M.insertWith' (+) k v m) m0 xs
{-# SPECIALIZE updateDefM :: [(String, Int)] -> M.Map String Int
                          -> M.Map String Int #-}
{-# SPECIALIZE updateDefM :: [(BS.ByteString, Int)] -> M.Map BS.ByteString Int
                          -> M.Map BS.ByteString Int #-}

deleteM :: Ord k => [k] -> M.Map k Int -> M.Map k Int
deleteM xs m0 = foldl' (\m k -> M.delete k m) m0 xs
{-# SPECIALIZE deleteM :: [String] -> M.Map String Int -> M.Map String Int #-}
{-# SPECIALIZE deleteM :: [BS.ByteString] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * IntMap

lookupIM :: [Int] -> IM.IntMap Int -> Int
lookupIM xs m = foldl' (\z k -> fromMaybe z (IM.lookup k m)) 0 xs

insertIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
insertIM xs m0 = foldl' (\m (k, v) -> IM.insert k v m) m0 xs

updateDefIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
updateDefIM xs m0 = foldl' (\m (k, v) -> IM.insertWith (+) k v m) m0 xs

deleteIM :: [Int] -> IM.IntMap Int -> IM.IntMap Int
deleteIM xs m0 = foldl' (\m k -> IM.delete k m) m0 xs
