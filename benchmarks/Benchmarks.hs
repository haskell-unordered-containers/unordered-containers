{-# LANGUAGE CPP, GADTs #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.Bits ((.&.))
import Data.Hashable (Hashable)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import qualified Util.ByteString as UBS
import qualified Util.Int as UI
import qualified Util.String as US

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString
#endif

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let hm   = HM.fromList elems :: HM.HashMap String Int
        hmbs = HM.fromList elemsBS :: HM.HashMap BS.ByteString Int
        hmi  = HM.fromList elemsI :: HM.HashMap Int Int
        hmi2 = HM.fromList elemsI2 :: HM.HashMap Int Int
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
          , bgroup "lookup-miss"
            [ bench "String" $ whnf (lookupM keys') m
            , bench "ByteString" $ whnf (lookupM keysBS') mbs
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insertM elems) M.empty
            , bench "ByteStringString" $ whnf (insertM elemsBS) M.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insertM elems) m
            , bench "ByteStringString" $ whnf (insertM elemsBS) mbs
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (deleteM keys) m
            , bench "ByteString" $ whnf (deleteM keysBS) mbs
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (deleteM keys') m
            , bench "ByteString" $ whnf (deleteM keysBS') mbs
            ]
          , bgroup "size"
            [ bench "String" $ whnf M.size m
            , bench "ByteString" $ whnf M.size mbs
            ]
          , bgroup "fromList"
            [ bench "String" $ whnf M.fromList elems
            , bench "ByteString" $ whnf M.fromList elemsBS
            ]
          ]

          -- ** IntMap
        , bgroup "IntMap"
          [ bench "lookup" $ whnf (lookupIM keysI) im
          , bench "lookup-miss" $ whnf (lookupIM keysI') im
          , bench "insert" $ whnf (insertIM elemsI) IM.empty
          , bench "insert-dup" $ whnf (insertIM elemsI) im
          , bench "delete" $ whnf (deleteIM keysI) im
          , bench "delete-miss" $ whnf (deleteIM keysI') im
          , bench "size" $ whnf IM.size im
          , bench "fromList" $ whnf IM.fromList elemsI
          ]

        , bgroup "HashMap"
          [ -- * Basic interface
            bgroup "lookup"
            [ bench "String" $ whnf (lookup keys) hm
            , bench "ByteString" $ whnf (lookup keysBS) hmbs
            , bench "Int" $ whnf (lookup keysI) hmi
            ]
          , bgroup "lookup-miss"
            [ bench "String" $ whnf (lookup keys') hm
            , bench "ByteString" $ whnf (lookup keysBS') hmbs
            , bench "Int" $ whnf (lookup keysI') hmi
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insert elems) HM.empty
            , bench "ByteString" $ whnf (insert elemsBS) HM.empty
            , bench "Int" $ whnf (insert elemsI) HM.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insert elems) hm
            , bench "ByteString" $ whnf (insert elemsBS) hmbs
            , bench "Int" $ whnf (insert elemsI) hmi
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (delete keys) hm
            , bench "ByteString" $ whnf (delete keysBS) hmbs
            , bench "Int" $ whnf (delete keysI) hmi
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (delete keys') hm
            , bench "ByteString" $ whnf (delete keysBS') hmbs
            , bench "Int" $ whnf (delete keysI') hmi
            ]

            -- Combine
          , bench "union" $ whnf (HM.union hmi) hmi2

            -- Transformations
          , bench "map" $ whnf (HM.map (\ v -> v + 1)) hmi

            -- * Difference and intersection
          , bench "difference" $ whnf (HM.difference hmi) hmi2
          , bench "intersection" $ whnf (HM.intersection hmi) hmi2

            -- Folds
          , bench "foldl'" $ whnf (HM.foldl' (+) 0) hmi
          , bench "foldr" $ nf (HM.foldr (:) []) hmi

            -- Filter
          , bench "filter" $ whnf (HM.filter (\ v -> v .&. 1 == 0)) hmi
          , bench "filterWithKey" $ whnf (HM.filterWithKey (\ k _ -> k .&. 1 == 0)) hmi

            -- Size
          , bgroup "size"
            [ bench "String" $ whnf HM.size hm
            , bench "ByteString" $ whnf HM.size hmbs
            , bench "Int" $ whnf HM.size hmi
            ]

            -- fromList
          , bgroup "fromList"
            [ bgroup name
              [ bgroup "long"
                [ bench "String" $ whnf fl1 elems
                , bench "ByteString" $ whnf fl2 elemsBS
                , bench "Int" $ whnf fl3 elemsI
                ]
              , bgroup "short"
                [ bench "String" $ whnf fl1 elemsDup
                , bench "ByteString" $ whnf fl2 elemsDupBS
                , bench "Int" $ whnf fl3 elemsDupI
                ]
              ]
            | (name,fl1,fl2,fl3)
                 <- [("Base",HM.fromList,HM.fromList,HM.fromList)
                    ,("insert",fromList_insert,fromList_insert,fromList_insert)]
            ]
            -- fromList
          , bgroup "fromListWith"
            [ bgroup name
              [ bgroup "long"
                [ bench "String" $ whnf (fl1 (+)) elems
                , bench "ByteString" $ whnf (fl2 (+)) elemsBS
                , bench "Int" $ whnf (fl3 (+)) elemsI
                ]
              , bgroup "short"
                [ bench "String" $ whnf (fl1 (+)) elemsDup
                , bench "ByteString" $ whnf (fl2 (+)) elemsDupBS
                , bench "Int" $ whnf (fl3 (+)) elemsDupI
                ]
              ]
            | (name,fl1,fl2,fl3)
                 <- [("Base",HM.fromListWith,HM.fromListWith,HM.fromListWith)
                    ,("insert",fromListWith_insert,fromListWith_insert,fromListWith_insert)]
            ]
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
    keysI   = UI.rnd (n+n) n
    elemsI2 = zip [n `div` 2..n + (n `div` 2)] [1..n]  -- for union

    keys'    = US.rnd' 8 n
    keysBS'  = UBS.rnd' 8 n
    keysI'   = UI.rnd' (n+n) n

    keysDup    = US.rnd 2 n
    keysDupBS  = UBS.rnd 2 n
    keysDupI   = UI.rnd (n`div`4) n
    elemsDup   = zip keysDup [1..n]
    elemsDupBS = zip keysDupBS [1..n]
    elemsDupI  = zip keysDupI [1..n]

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

delete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> HM.HashMap k Int
delete xs m0 = foldl' (\m k -> HM.delete k m) m0 xs
{-# SPECIALIZE delete :: [Int] -> HM.HashMap Int Int -> HM.HashMap Int Int #-}
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
-- * IntMap

lookupIM :: [Int] -> IM.IntMap Int -> Int
lookupIM xs m = foldl' (\z k -> fromMaybe z (IM.lookup k m)) 0 xs

insertIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
insertIM xs m0 = foldl' (\m (k, v) -> IM.insert k v m) m0 xs

deleteIM :: [Int] -> IM.IntMap Int -> IM.IntMap Int
deleteIM xs m0 = foldl' (\m k -> IM.delete k m) m0 xs

------------------------------------------------------------------------
-- * Reference implementations

fromList_insert :: (Eq k, Hashable k) => [(k, v)] -> HM.HashMap k v
fromList_insert = foldl' (\ m (k, v) -> HM.insert k v m) HM.empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromList_insert #-}
#endif

fromListWith_insert :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HM.HashMap k v
fromListWith_insert f = foldl' (\ m (k, v) -> HM.insertWith f k v m) HM.empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromListWith_insert #-}
#endif
