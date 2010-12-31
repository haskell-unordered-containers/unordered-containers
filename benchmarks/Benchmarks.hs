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
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import qualified Util.String as US
import qualified Util.ByteString as UBS

instance NFData BS.ByteString

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let hm   = fromList elems :: HM.HashMap String Int
        hmbs = fromList elemsBS :: HM.HashMap BS.ByteString Int
        m    = M.fromList elems :: M.Map String Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [B m, B hm, B hmbs])
        [ bgroup "lookup"
          [ bgroup "HashMap"
            [ bench "String" $ nf (lookup keys) hm
            , bench "ByteString" $ nf (lookup keysBS) hmbs
            ]
          , bench "Map" $ nf (lookupM keys) m
          ]
        , bgroup "insert"
          [ bgroup "HashMap"
            [ bench "String" $ nf (insert elems) HM.empty
            , bench "ByteString" $ nf (insert elemsBS) HM.empty
            ]
          , bench "Map" $ nf (insertM elems) M.empty
          ]
        , bgroup "delete"
          [ bench "HashMap" $ nf (delete keys) hm
          , bench "Map" $ nf (insertM elems) M.empty
          ]
        ]
  where
    n :: Int
    n = 2^(12 :: Int)

    elems = zip keys [1..n]
    keys = US.rnd 8 n
    elemsBS = zip keysBS [1..n]
    keysBS = UBS.rnd 8 n

------------------------------------------------------------------------
-- * HashMap

lookup :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (HM.lookup k m)) 0 xs
{-# SPECIALIZE lookup :: [String] -> HM.HashMap String Int -> Int #-}
{-# SPECIALIZE lookup :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> Int #-}

insert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
       -> HM.HashMap k Int
insert xs m0 = foldl' (\m (k, v) -> HM.insert k v m) m0 xs
{-# SPECIALIZE insert :: [(String, Int)] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE insert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

delete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> HM.HashMap k Int
delete xs m0 = foldl' (\m k -> HM.delete k m) m0 xs
{-# SPECIALIZE delete :: [String] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE delete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

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

deleteM :: Ord k => [k] -> M.Map k Int -> M.Map k Int
deleteM xs m0 = foldl' (\m k -> M.delete k m) m0 xs
{-# SPECIALIZE deleteM :: [String] -> M.Map String Int -> M.Map String Int #-}
{-# SPECIALIZE deleteM :: [BS.ByteString] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * Helpers

fromList :: (Eq k, Hashable k) => [(k, v)] -> HM.HashMap k v
fromList = foldl' (\m (k, v) -> HM.insert k v m) HM.empty
