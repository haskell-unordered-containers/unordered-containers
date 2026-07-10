{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the invariant that 'Data.HashMap.Strict' operations don't
-- leave thunks behind, given thunk-free inputs (#235).
module Properties.NoThunks (tests) where

import Control.DeepSeq         (NFData, rnf)
import Properties.HashMapStrict ()
import Test.QuickCheck          (ioProperty)
import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.QuickCheck   (testProperty)
import Util.Key                 (Key, incKey, keyToInt)
import Util.NoThunks            (noThunksProperty)

import qualified Data.HashMap.Strict as HM

type HMK  = HM.HashMap Key
type HMKI = HMK Int

-- | Force a value to normal form before using it as test input: the
-- no-thunks invariant is only claimed for thunk-free inputs.
forced :: NFData a => a -> a
forced x = rnf x `seq` x

tests :: TestTree
tests = testGroup "NoThunks"
  [ testGroup "singleton"
    [ testProperty "no thunks" $
      \(k :: Key) (v :: Int) -> noThunksProperty (HM.singleton (forced k) (forced v))
    ]
  , testGroup "insert"
    [ testProperty "no thunks" $
      \(k :: Key) (v :: Int) (m :: HMKI) ->
        noThunksProperty (HM.insert (forced k) (forced v) (forced m))
    ]
  , testGroup "insertWith"
    [ testProperty "no thunks" $
      \(k :: Key) (v :: Int) (m :: HMKI) ->
        noThunksProperty (HM.insertWith (+) (forced k) (forced v) (forced m))
    ]
  , testGroup "delete"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) -> noThunksProperty (HM.delete (forced k) (forced m))
    ]
  , testGroup "adjust"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) -> noThunksProperty (HM.adjust (+ 1) (forced k) (forced m))
    ]
  , testGroup "update"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        noThunksProperty (HM.update (\v -> Just (v + 1)) (forced k) (forced m))
    ]
  , testGroup "alter"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        noThunksProperty (HM.alter (\mv -> Just (maybe 0 (+ 1) mv)) (forced k) (forced m))
    ]
  , testGroup "alterF"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        let MyIdentity m' =
              HM.alterF (MyIdentity . Just . maybe 0 (+ 1)) (forced k) (forced m)
        in  noThunksProperty m'
    ]
  , testGroup "union"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (HM.union (forced m1) (forced m2))
    ]
  , testGroup "unionWith"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty (HM.unionWith (+) (forced m1) (forced m2))
    ]
  , testGroup "unionWithKey"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.unionWithKey (\k v1 v2 -> keyToInt k + v1 + v2) (forced m1) (forced m2))
    ]
  , testGroup "unions"
    [ testProperty "no thunks" $
      \(ms :: [HMKI]) -> noThunksProperty (HM.unions (forced ms))
    ]
  , testGroup "map"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.map (+ 1) (forced m))
    ]
  , testGroup "mapWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.mapWithKey (\k v -> keyToInt k + v) (forced m))
    ]
  , testGroup "traverseWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> ioProperty $ do
        m' <- HM.traverseWithKey (\k v -> pure (keyToInt k + v)) (forced m)
        pure (noThunksProperty m')
    ]
  , testGroup "mapKeys"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.mapKeys incKey (forced m))
    ]
  , testGroup "difference"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (HM.difference (forced m1) (forced m2))
    ]
  , testGroup "differenceWith"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty (HM.differenceWith (\v1 v2 -> Just (v1 + v2)) (forced m1) (forced m2))
    ]
  , testGroup "intersection"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (HM.intersection (forced m1) (forced m2))
    ]
  , testGroup "intersectionWith"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty (HM.intersectionWith (+) (forced m1) (forced m2))
    ]
  , testGroup "intersectionWithKey"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.intersectionWithKey (\k v1 v2 -> keyToInt k + v1 + v2) (forced m1) (forced m2))
    ]
  , testGroup "filter"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.filter even (forced m))
    ]
  , testGroup "filterWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty (HM.filterWithKey (\k v -> even (keyToInt k + v)) (forced m))
    ]
  , testGroup "mapMaybe"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty
          (HM.mapMaybe (\v -> if even v then Just (v + 1) else Nothing) (forced m))
    ]
  , testGroup "mapMaybeWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty $
          HM.mapMaybeWithKey
            (\k v -> if even v then Just (keyToInt k + v) else Nothing)
            (forced m)
    ]
  , testGroup "fromList"
    [ testProperty "no thunks" $
      \(kvs :: [(Key, Int)]) -> noThunksProperty (HM.fromList (forced kvs))
    ]
  , testGroup "fromListWith"
    [ testProperty "no thunks" $
      \(kvs :: [(Key, Int)]) -> noThunksProperty (HM.fromListWith (+) (forced kvs))
    ]
  , testGroup "fromListWithKey"
    [ testProperty "no thunks" $
      \(kvs :: [(Key, Int)]) ->
        noThunksProperty (HM.fromListWithKey (\k v1 v2 -> keyToInt k + v1 + v2) (forced kvs))
    ]
  , testGroup "<>"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (forced m1 <> forced m2)
    ]
  ]

-- Custom Functor to prevent interference from 'alterF' rules.
newtype MyIdentity a = MyIdentity a
instance Functor MyIdentity where
  fmap f (MyIdentity x) = MyIdentity (f x)
