{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-} -- because of Arbitrary (HashMap k v)

-- | Tests for size field invariant in @HashMap@ wrapper introduced in GitHub
-- PR #170.

module Properties.Size (tests) where

import Data.Maybe                    (isJust, isNothing)
import Data.Hashable                 (Hashable)
#if defined(STRICT)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
#else
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as HM
#endif
import qualified Data.Map            as M

import GHC.Generics                  (Generic)

import Test.QuickCheck               (Arbitrary (..), Property, conjoin, frequency, (===),
                                      genericShrink)
import Test.Tasty                    (TestTree, testGroup)
import Test.Tasty.QuickCheck         (testProperty)
import Util.Key                      (Key (..), keyToInt)

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary
  shrink = fmap HM.fromList . shrink . HM.toList

-- | Property to check that the hashmap built by @fromList@ applied to a list
-- without repeating keys will have the right size i.e. equal to the list's
-- length.
fromListProperty :: M.Map Key Int -> Bool
fromListProperty m =
    let sz   = M.size m
        list = M.toList m
        hm   = HM.fromList list
    in sz == HM.size hm

-- | Property to check that the hashmap built by @fromListWith@ applied to a
--list without repeating keys will have the right size i.e. equal to the list's
-- length.
fromListWithProperty :: M.Map Key Int -> Bool
fromListWithProperty m =
    let sz   = M.size m
        list = M.toList m
        hm   = HM.fromListWith (+) list
    in sz == HM.size hm

{- Note on @HashMapAction@ datatype
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some actions correspond to functions from @Data.HashMap.Base@ that require
function arguments i.e. @insertWith@ requires a @v -> v -> v@ argument.
However, function values do not have a @Show@ instance, which is undesirable because if QuickCheck fails it'll print the values for which a certain test failed.
To get around this, simple functions like @(+)@ are used instead.

Furthermore, when functions have a @Bool@ or a @Maybe v@ argument and/or
result value, simple predicates like @even/odd@ are used to "mimic" such
functions. An example: @mapMaybe@ has an argument @f::(a -> Maybe b)@, but in
these tests all hashmaps are instantiated as @HashMap Key Int@, so no parameter
is passed to @MapMaybe@ in @HashMapAction@ and @f = \v -> if odd v then Just
(succ v) else Nothing@ is used instead.
-}

-- Datatype representing the actions that can potentially change a hashmap's
-- size.
data HashMapAction
    = Insert Key Int
    | InsertWith Key Int
    | Adjust Key
    | Update (Maybe Int) Key
    | Alter (Maybe Int) Key
    | Delete Key
    | Union (HM.HashMap Key Int)
    | UnionWith (HM.HashMap Key Int)
    | UnionWithKey (HM.HashMap Key Int)
    | Intersection (HM.HashMap Key Int)
    | IntersectionWith (HM.HashMap Key Int)
    | IntersectionWithKey (HM.HashMap Key Int)
    | Difference (HM.HashMap Key Int)
    | DifferenceWith (HM.HashMap Key Int)
    | Filter
    | FilterWithKey
    | Map
    | MapMaybe
    | MapMaybeWithKey
  deriving (Eq, Show, Generic)

-- Here, higher weights are used for operations that increase the size of the
-- hashmap so that its size is more likely to grow instead of nearing and
-- staying 0, creating more interesting sequences of actions to be tested.
instance Arbitrary HashMapAction where
    arbitrary = frequency
        [ (4, Insert <$> arbitrary <*> arbitrary)
        , (4, InsertWith <$> arbitrary <*> arbitrary)
        , (4, Union <$> arbitrary)
        , (4, UnionWith <$> arbitrary)
        , (4, UnionWithKey <$> arbitrary)
        , (1, Adjust <$> arbitrary)
        , (1, Update <$> arbitrary <*> arbitrary)
        , (1, Alter <$> arbitrary <*> arbitrary)
        , (1, Delete <$> arbitrary)
        , (1, Intersection <$> arbitrary)
        , (1, IntersectionWith <$> arbitrary)
        , (1, IntersectionWithKey <$> arbitrary)
        , (1, Difference <$> arbitrary)
        , (1, DifferenceWith <$> arbitrary)
        , (1, pure Filter)
        , (1, pure FilterWithKey)
        , (1, pure Map)
        , (1, pure MapMaybe)
        , (1, pure MapMaybeWithKey)
        ]
    shrink = genericShrink

-- Simple way of representing a hashmap and its size without having to
-- use @size@, which is the function to be tested. As such, its use is
-- avoided and the @Int@ field of the tuple is used instead.
data HashMapState = HashMapState
    { sz :: Int                -- ^ The size of the @hm@ hashmap, also in this
                               -- datatype, obtained without using @size@.
    , hm :: HM.HashMap Key Int -- ^ The hashmap resultant of every
                               -- @HashMapAction@ from the start of the test.
    } deriving (Show, Eq)

-- | Applies a @HashMapAction@ to @HashMapState@, updating the hashmap's
-- size after the operation.
applyActionToState :: HashMapState -> HashMapAction -> HashMapState
applyActionToState HashMapState {..} (Insert k v)
    | HM.member k hm = HashMapState sz hm'
    | otherwise      = HashMapState (sz + 1) hm'
  where
    hm' = HM.insert k v hm
applyActionToState HashMapState {..} (InsertWith k v)
    | HM.member k hm = HashMapState sz hm'
    | otherwise      = HashMapState (sz + 1) hm'
  where
    hm' = HM.insertWith (+) k v hm
applyActionToState HashMapState {..} (Adjust k) = HashMapState sz (HM.adjust succ k hm)
applyActionToState HashMapState {..} (Update mk k)
    | HM.member k hm && isNothing mk = HashMapState (sz - 1) hm'
    | otherwise = HashMapState sz hm'
  where
    hm' = HM.update (const mk) k hm
applyActionToState HashMapState {..} (Alter mv k) =
    case (HM.member k hm, mv) of
        (True, Just _)   -> HashMapState sz hm'
        (True, Nothing)  -> HashMapState (sz - 1) hm'
        (False, Just _)  -> HashMapState (sz + 1) hm'
        (False, Nothing) -> HashMapState sz hm'
  where
    func = const mv
    hm' = HM.alter func k hm
applyActionToState HashMapState {..} (Delete k)
    | HM.member k hm = HashMapState (sz - 1) hm'
    | otherwise      = HashMapState sz hm'
  where
    hm' = HM.delete k hm
applyActionToState HashMapState {..} (Union hm') =
    let sz'          = length $ HM.toList hm'
        lenIntersect = length [ k | k <- HM.keys hm, HM.member k hm' ]
        newLen       = sz + sz' - lenIntersect
    in HashMapState newLen (HM.union hm hm')
applyActionToState HashMapState {..} (UnionWith hm') =
    let sz'          = length $ HM.toList hm'
        lenIntersect = length [ k | k <- HM.keys hm, HM.member k hm' ]
        newLen       = sz + sz' - lenIntersect
    in HashMapState newLen  (HM.unionWith (+) hm hm')
applyActionToState HashMapState {..} (UnionWithKey hm') =
    let sz'          = length $ HM.toList hm'
        lenIntersect = length [ k | k <- HM.keys hm, HM.member k hm' ]
        newLen       = sz + sz' - lenIntersect
        fun k v1 v2  = keyToInt k + v1 + v2
    in HashMapState newLen (HM.unionWithKey fun hm hm')
applyActionToState HashMapState {..} (Intersection hm') =
    let lenIntersect = length [ k | k <- HM.keys hm, HM.member k hm' ]
    in HashMapState lenIntersect (HM.intersection hm hm')
applyActionToState HashMapState {..} (IntersectionWith hm') =
    let lenIntersect = length [ k | k <- HM.keys hm, HM.member k hm' ]
    in HashMapState lenIntersect (HM.intersectionWith (+) hm hm')
applyActionToState HashMapState {..} (IntersectionWithKey hm') =
    let lenIntersect = length [ k | k <- HM.keys hm, HM.member k hm' ]
        fun k v1 v2  = keyToInt k + v1 + v2
    in HashMapState lenIntersect (HM.intersectionWithKey fun hm hm')
applyActionToState HashMapState {..} (Difference hm') =
    let lenDiff = length [ k | k <- HM.keys hm, not $ HM.member k hm' ]
    in HashMapState lenDiff (HM.difference hm hm')
applyActionToState HashMapState {..} (DifferenceWith hm') =
    let fun v w = if odd v then Just (v + w) else Nothing
        lenDiff = length [ k | (k, v) <- HM.toList hm, not $ HM.member k hm' && even v]
    in HashMapState lenDiff (HM.differenceWith fun hm hm')
applyActionToState HashMapState {..} Filter =
    let lenFilter = length [ (k, v) | (k, v) <- HM.toList hm, even v ]
    in HashMapState lenFilter (HM.filter even hm)
applyActionToState HashMapState {..} FilterWithKey =
    let lenFilter = length [ (k, v) | (k, v) <- HM.toList hm, even $ keyToInt k + v ]
    in HashMapState lenFilter (HM.filterWithKey (\k -> even . (+) (keyToInt k)) hm)
applyActionToState HashMapState {..} Map = HashMapState sz (HM.map succ hm)
applyActionToState HashMapState {..} MapMaybe =
    let mapFun v = if odd v then Just (succ v) else Nothing
        lenMapMaybe = length [ (k, v) | (k, v) <- HM.toList hm, isJust (mapFun v)]
    in HashMapState lenMapMaybe (HM.mapMaybe mapFun hm)
applyActionToState HashMapState {..} MapMaybeWithKey =
    let mapFun k v = if odd v then Just (keyToInt k + succ v) else Nothing
        lenMapMaybe = length [ (k, v) | (k, v) <- HM.toList hm, isJust (mapFun k v)]
    in HashMapState lenMapMaybe (HM.mapMaybeWithKey mapFun hm)

-- | Property to check that after each operation that may change a hashmap's
-- size, the @Int@ field in the @HashMap@ wrapper always correctly represents
-- the hashmap's size.
sizeInvariantProperty :: [HashMapAction] -> Property
sizeInvariantProperty actionList =
    conjoin .
    map (\HashMapState {..} -> sz === HM.size hm) .
    scanl applyActionToState (HashMapState 0 mempty) $ actionList

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests = testGroup "Data.HashMap.size" [
          testGroup "size invariant checks"
          [ testProperty "size" sizeInvariantProperty
          , testProperty "fromList" fromListProperty
          , testProperty "fromListWith" fromListWithProperty
          ]
        ]