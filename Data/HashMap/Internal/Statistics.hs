{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.HashMap.Internal.Statistics where

import qualified Data.HashMap.Internal as HM
import qualified Data.HashMap.Internal.Array as A
import Data.HashMap.Internal (HashMap(..))

data Stats = S {
      size :: !Int
    , nodeCounts :: A.Array NodeCount
    , collisions :: HashMap Int [HM.Hash]
    }

data NodeCount = NC {
      empty         :: !Int
    , leaf          :: !Int
    , bitmapIndexed :: !Int
    , full          :: !Int
    , collision     :: !Int
    } deriving Show

instance Semigroup NodeCount where
    h1 <> h2 = NC {
          empty         = empty h1 + empty h2
        , leaf          = leaf h1 + leaf h2
        , bitmapIndexed = bitmapIndexed h1 + bitmapIndexed h2
        , full          = full h1 + full h2
        , collision     = collision h1 + collision h2
        }

instance Monoid NodeCount where
    mempty = NC 0 0 0 0 0

-- | Count the number of node types at each level
nodeNodeCount :: HM.HashMap k v -> [NodeCount]
nodeNodeCount Empty = [mempty { empty = 1 }]
nodeNodeCount (Leaf {}) = [mempty { leaf = 1 }]
nodeNodeCount (BitmapIndexed _ ary) =
    mempty { bitmapIndexed = 1 } :
    A.foldl' (\ xs -> zipWith_ merge xs . nodeNodeCount) [] ary
nodeNodeCount (Full ary) =
    mempty { full = 1 } :
    A.foldl' (\ xs -> zipWith_ merge xs . nodeNodeCount) [] ary
nodeNodeCount (Collision {}) = [mempty { collision = 1 }]

merge :: Monoid a => Maybe a -> Maybe a -> a
merge (Just h1) (Just h2) = h1 `mappend` h2
merge (Just h) Nothing    = h `mappend` mempty
merge Nothing (Just h)    = mempty `mappend` h
merge Nothing Nothing     = error "impossible"

zipWith_ :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWith_ f = go
  where
    go [] []         = []
    go [] (y:ys)     = let z = f Nothing (Just y) in z `seq` (z : go [] ys)
    go (x:xs) []     = let z = f (Just x) Nothing in z `seq` (z : go xs [])
    go (x:xs) (y:ys) = let z = f (Just x) (Just y) in z `seq` (z : go xs ys)
{-# INLINE zipWith_ #-}

-- mkStats :: HashMap k v ->
