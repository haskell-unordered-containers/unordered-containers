{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the invariant that 'Data.HashMap.Strict' operations don't
-- leave thunks behind, given thunk-free inputs (#235).
module Properties.NoThunks (tests) where

import Control.DeepSeq         (NFData, rnf)
import Data.Functor.Identity   (Identity (..))
import Properties.HashMapStrict ()
import Test.QuickCheck          (ioProperty)
import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.QuickCheck   (testProperty)
import Util.Key                 (Key, incKey, keyToInt)
import Util.NoThunks            (noThunksProperty)

import qualified Data.HashMap.Strict as HM

type HMK  = HM.HashMap Key
type HMKI = HMK Int
type HMKK = HMK Key

-- | Force a value to normal form before using it as test input: the
-- no-thunks invariant is only claimed for thunk-free inputs.
forced :: NFData a => a -> a
forced x = rnf x `seq` x

opaqueSucc :: Int -> Int
opaqueSucc x = x + 1
{-# NOINLINE opaqueSucc #-}

opaque1 :: (a -> b) -> a -> b
opaque1 f x = f x
{-# NOINLINE opaque1 #-}

opaque2 :: (a -> b -> c) -> a -> b -> c
opaque2 f x y = f x y
{-# NOINLINE opaque2 #-}

opaque3 :: (a -> b -> c -> d) -> a -> b -> c -> d
opaque3 f x y z = f x y z
{-# NOINLINE opaque3 #-}

tests :: TestTree
tests = testGroup "NoThunks"
  [ testGroup "empty"
    [ testProperty "no thunks" $
        noThunksProperty (HM.empty :: HMKI)
    ]
  , testGroup "singleton"
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
        noThunksProperty (HM.insertWith (opaque2 (+)) (forced k) (forced v) (forced m))
    ]
  , testGroup "delete"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) -> noThunksProperty (HM.delete (forced k) (forced m))
    ]
  , testGroup "adjust"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        noThunksProperty (HM.adjust (opaque1 (+ 1)) (forced k) (forced m))
    ]
  , testGroup "update"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        noThunksProperty
          (HM.update (opaque1 (\v -> Just (v + 1))) (forced k) (forced m))
    ]
  , testGroup "alter"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        noThunksProperty
          (HM.alter
            (opaque1 (\mv -> Just (maybe 0 (+ 1) mv)))
            (forced k)
            (forced m))
    ]
  , testGroup "alterF"
    [ testProperty "no thunks" $
      \(k :: Key) (m :: HMKI) ->
        let MyIdentity m' =
              HM.alterF
                (opaque1 (MyIdentity . Just . maybe 0 (+ 1)))
                (forced k)
                (forced m)
        in  noThunksProperty m'
    -- Identity and the always-Just result make alterFinsertWith fire.
    , testProperty "alterFinsertWith rule: no thunks" $
      \(k :: Key) (v :: Int) (m :: HMKI) ->
        let Identity m' =
              HM.alterF
                (Identity . Just . maybe (opaqueSucc (forced v)) opaqueSucc)
                (forced k)
                (forced m)
        in  noThunksProperty m'
    -- Ignoring the old value makes alterFconstant fire.
    , testProperty "alterFconstant rule: no thunks" $
      \(k :: Key) (mv :: Maybe Int) (m :: HMKI) ->
        let Identity m' =
              HM.alterF
                (const (Identity (forced mv)))
                (forced k)
                (forced m)
        in  noThunksProperty m'
    -- Preserving absence makes alterFadjust fire.
    , testProperty "alterFadjust rule: no thunks" $
      \(k :: Key) (m :: HMKI) ->
        let Identity m' =
              HM.alterF
                (Identity . fmap opaqueSucc)
                (forced k)
                (forced m)
        in  noThunksProperty m'
    ]
  , testGroup "union"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (HM.union (forced m1) (forced m2))
    ]
  , testGroup "unionWith"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty (HM.unionWith (opaque2 (+)) (forced m1) (forced m2))
    ]
  , testGroup "unionWithKey"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.unionWithKey
            (opaque3 (\k v1 v2 -> keyToInt k + v1 + v2))
            (forced m1)
            (forced m2))
    ]
  , testGroup "unions"
    [ testProperty "no thunks" $
      \(ms :: [HMKI]) -> noThunksProperty (HM.unions (forced ms))
    ]
  , testGroup "map"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.map (opaque1 (+ 1)) (forced m))
    ]
  , testGroup "mapWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty
          (HM.mapWithKey (opaque2 (\k v -> keyToInt k + v)) (forced m))
    ]
  , testGroup "traverseWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> ioProperty $ do
        m' <- HM.traverseWithKey
          (opaque2 (\k v -> pure (keyToInt k + v)))
          (forced m)
        pure (noThunksProperty m')
    ]
  , testGroup "mapKeys"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.mapKeys (opaque1 incKey) (forced m))
    ]
  , testGroup "compose"
    [ testProperty "no thunks" $
      \(bc :: HMKI) (ab :: HMKK) ->
        noThunksProperty (HM.compose (forced bc) (forced ab))
    ]
  , testGroup "difference"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (HM.difference (forced m1) (forced m2))
    ]
  , testGroup "differenceWith"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.differenceWith
            (opaque2 (\v1 v2 -> Just (v1 + v2)))
            (forced m1)
            (forced m2))
    ]
  , testGroup "differenceWithKey"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.differenceWithKey
            (opaque3 (\k v1 v2 -> Just (keyToInt k + v1 + v2)))
            (forced m1)
            (forced m2))
    ]
  , testGroup "intersection"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) -> noThunksProperty (HM.intersection (forced m1) (forced m2))
    ]
  , testGroup "intersectionWith"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.intersectionWith (opaque2 (+)) (forced m1) (forced m2))
    ]
  , testGroup "intersectionWithKey"
    [ testProperty "no thunks" $
      \(m1 :: HMKI) (m2 :: HMKI) ->
        noThunksProperty
          (HM.intersectionWithKey
            (opaque3 (\k v1 v2 -> keyToInt k + v1 + v2))
            (forced m1)
            (forced m2))
    ]
  , testGroup "filter"
    [ testProperty "no thunks" $
      \(m :: HMKI) -> noThunksProperty (HM.filter (opaque1 even) (forced m))
    ]
  , testGroup "filterWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty
          (HM.filterWithKey
            (opaque2 (\k v -> even (keyToInt k + v)))
            (forced m))
    ]
  , testGroup "mapMaybe"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty
          (HM.mapMaybe
            (opaque1 (\v -> if even v then Just (v + 1) else Nothing))
            (forced m))
    ]
  , testGroup "mapMaybeWithKey"
    [ testProperty "no thunks" $
      \(m :: HMKI) ->
        noThunksProperty $
          HM.mapMaybeWithKey
            (opaque2
              (\k v -> if even v then Just (keyToInt k + v) else Nothing))
            (forced m)
    ]
  , testGroup "fromList"
    [ testProperty "no thunks" $
      \(kvs :: [(Key, Int)]) -> noThunksProperty (HM.fromList (forced kvs))
    ]
  , testGroup "fromListWith"
    [ testProperty "no thunks" $
      \(kvs :: [(Key, Int)]) ->
        noThunksProperty (HM.fromListWith (opaque2 (+)) (forced kvs))
    ]
  , testGroup "fromListWithKey"
    [ testProperty "no thunks" $
      \(kvs :: [(Key, Int)]) ->
        noThunksProperty
          (HM.fromListWithKey
            (opaque3 (\k v1 v2 -> keyToInt k + v1 + v2))
            (forced kvs))
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
