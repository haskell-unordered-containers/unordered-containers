{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- because of Arbitrary (HashMap k v)

-- | Tests for the 'Data.HashMap.Lazy' module.  We test functions by
-- comparing them to @Map@ from @containers@.

#if defined(STRICT)
#define MODULE_NAME Properties.HashMapStrict
#else
#define MODULE_NAME Properties.HashMapLazy
#endif

module MODULE_NAME (tests) where

import Control.Applicative         (Const (..))
import Data.Bifoldable
import Data.Function               (on)
import Data.Functor.Identity       (Identity (..))
import Data.Hashable               (Hashable (hashWithSalt))
import Data.HashMap.Internal.Debug (Validity (..), valid)
import Data.Ord                    (comparing)
import Test.QuickCheck             (Arbitrary (..), Property, elements, forAll,
                                    property, (===), (==>))
import Test.QuickCheck.Function    (Fun, apply, applyFun2, applyFun3)
import Test.QuickCheck.Poly        (A, B, C)
import Test.Tasty                  (TestTree, testGroup)
import Test.Tasty.QuickCheck       (testProperty)
import Util.Key                    (Key, keyToInt, incKey)

import qualified Test.QuickCheck as QC
import qualified Data.Foldable as Foldable
import qualified Data.List     as List

#if defined(STRICT)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
#else
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy     as M
#endif

instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary
  shrink = fmap HM.fromList . shrink . HM.toList

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Folds

pBifoldMap :: [(Int, Int)] -> Property
pBifoldMap xs = concatMap f (HM.toList m) === bifoldMap (:[]) (:[]) m
  where f (k, v) = [k, v]
        m = HM.fromList xs

pBifoldr :: [(Int, Int)] -> Property
pBifoldr xs = concatMap f (HM.toList m) === bifoldr (:) (:) [] m
  where f (k, v) = [k, v]
        m = HM.fromList xs

pBifoldl :: [(Int, Int)] -> Property
pBifoldl xs = reverse (concatMap f $ HM.toList m) === bifoldl (flip (:)) (flip (:)) [] m
  where f (k, v) = [k, v]
        m = HM.fromList xs

------------------------------------------------------------------------
-- ** Conversions

-- The free magma is used to test that operations are applied in the
-- same order.
data Magma a
  = Leaf a
  | Op (Magma a) (Magma a)
  deriving (Show, Eq, Ord)

instance Hashable a => Hashable (Magma a) where
  hashWithSalt s (Leaf a) = hashWithSalt s (hashWithSalt (1::Int) a)
  hashWithSalt s (Op m n) = hashWithSalt s (hashWithSalt (hashWithSalt (2::Int) m) n)

-- 'eq_' already calls fromList.
pFromList :: [(Key, Int)] -> Property
pFromList = id `eq_` id

pFromListValid :: [(Key, ())] -> Property
pFromListValid xs = valid (HM.fromList xs) === Valid

pFromListWith :: [(Key, Int)] -> Property
pFromListWith kvs = (M.toAscList $ M.fromListWith Op kvsM) ===
                    (toAscList $ HM.fromListWith Op kvsM)
  where kvsM = fmap (fmap Leaf) kvs

pFromListWithKey :: [(Key, Int)] -> Property
pFromListWithKey kvs = (M.toAscList $ M.fromListWithKey combine kvsM) ===
                       (toAscList $ HM.fromListWithKey combine kvsM)
  where kvsM = fmap (\(k,v) -> (Leaf (keyToInt k), Leaf v)) kvs
        combine k v1 v2 = Op k (Op v1 v2)

pToList :: [(Key, Int)] -> Property
pToList = M.toAscList `eq` toAscList

pElems :: [(Key, Int)] -> Property
pElems = (List.sort . M.elems) `eq` (List.sort . HM.elems)

pKeys :: [(Key, Int)] -> Property
pKeys = (List.sort . M.keys) `eq` (List.sort . HM.keys)

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests =
  testGroup
#if defined(STRICT)
    "Data.HashMap.Strict"
#else
    "Data.HashMap.Lazy"
#endif
    [
    -- Instances
      testGroup "instances"
      [ testGroup "Eq"
        [ testProperty "==" $
          \(x :: HMKI) y -> (x == y) === (toOrdMap x == toOrdMap y)
        , testProperty "/=" $
          \(x :: HMKI) y -> (x == y) === (toOrdMap x == toOrdMap y)
        ]
      , testGroup "Ord"
        [ testProperty "compare reflexive" $
          \(m :: HMKI) -> compare m m === EQ
        , testProperty "compare transitive" $
          \(x :: HMKI) y z -> case (compare x y, compare y z) of
            (EQ, o)  -> compare x z === o
            (o,  EQ) -> compare x z === o
            (LT, LT) -> compare x z === LT
            (GT, GT) -> compare x z === GT
            (LT, GT) -> property True -- ys greater than xs and zs.
            (GT, LT) -> property True
        , testProperty "compare antisymmetric" $
          \(x :: HMKI) y -> case (compare x y, compare y x) of
            (EQ, EQ) -> True
            (LT, GT) -> True
            (GT, LT) -> True
            _        -> False
        , testProperty "Ord => Eq" $
          \(x :: HMKI) y -> case (compare x y, x == y) of
            (EQ, True)  -> True
            (LT, False) -> True
            (GT, False) -> True
            _           -> False
        ]
      , testProperty "Read/Show" $
        \(x :: HMKI) -> x === read (show x)
      , testProperty "Functor" $
        \(x :: HMKI) (f :: Fun Int Int) ->
          let y = fmap (apply f) x
          in  toOrdMap y === fmap (apply f) (toOrdMap x)
      , testProperty "Foldable" $
        \(x :: HMKI) ->
          let f = List.sort . Foldable.foldr (:) []
          in  f x === f (toOrdMap x)
      , testGroup "Bifoldable"
        [ testProperty "bifoldMap" pBifoldMap
        , testProperty "bifoldr" pBifoldr
        , testProperty "bifoldl" pBifoldl
        ]
      , testProperty "Hashable" $
        \(xs :: [(Key, Int)]) is salt ->
          let xs' = List.nubBy (\(k,_) (k',_) -> k == k') xs
              -- Shuffle the list using indexes in the second
              shuffle :: [Int] -> [a] -> [a]
              shuffle idxs = List.map snd
                           . List.sortBy (comparing fst)
                           . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
              ys = shuffle is xs'
              x = HM.fromList xs'
              y = HM.fromList ys
          in  x == y ==> hashWithSalt salt x === hashWithSalt salt y
      ]
    -- Basic interface
    , testProperty "size" $
      \(x :: HMKI) -> HM.size x === M.size (toOrdMap x)
    , testProperty "member" $
      \(k :: Key) (m :: HMKI) -> HM.member k m === M.member k (toOrdMap m)
    , testProperty "lookup" $
      \(k :: Key) (m :: HMKI) -> HM.lookup k m === M.lookup k (toOrdMap m)
    , testProperty "!?" $
      \(k :: Key) (m :: HMKI) -> m HM.!? k === M.lookup k (toOrdMap m)
    , testProperty "insert" $
      \(k :: Key) (v :: Int) x ->
        let y = HM.insert k v x
        in  toOrdMap y === M.insert k v (toOrdMap x)
    , testProperty "delete" $
      \(k :: Key) (x :: HMKI) ->
      let y = HM.delete k x
      in  toOrdMap y === M.delete k (toOrdMap x)
    , testProperty "insertWith" $
      \f k v (x :: HMKI) ->
        let y = HM.insertWith (applyFun2 f) k v x
        in  toOrdMap y === M.insertWith (applyFun2 f) k v (toOrdMap x)
    , testProperty "adjust" $
      \f k (x :: HMKI) ->
        let y = HM.adjust (apply f) k x
        in  toOrdMap y === M.adjust (apply f) k (toOrdMap x)
    , testProperty "update" $
      \f k (x :: HMKI) ->
        let y = HM.update (apply f) k x
        in  toOrdMap y === M.update (apply f) k (toOrdMap x)
    , testProperty "alter" $
      \f k (x :: HMKI) ->
        let y = HM.alter (apply f) k x
        in  toOrdMap y === M.alter (apply f) k (toOrdMap x)
    , testGroup "alterF"
      [ -- We choose the list functor here because we don't fuss with
        -- it in alterF rules and because it has a sufficiently interesting
        -- structure to have a good chance of breaking if something is wrong.
        testProperty "[]" $
        \(f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
          let ys = HM.alterF (apply f) k x
          in  map toOrdMap ys === M.alterF (apply f) k (toOrdMap x)
      , testProperty "adjust" $
        \f k (x :: HMKI) ->
          let g = Identity . fmap (apply f)
              y = HM.alterF g k x
          in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
      , testProperty "insert" $
        \v k (x :: HMKI) ->
          let g = const . Identity . Just $ v
              y = HM.alterF g k x
          in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
      , testProperty "insertWith" $
        \f k v (x :: HMKI) ->
          let g = Identity . Just . maybe v (apply f)
              y = HM.alterF g k x
          in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
      , testProperty "delete" $
        \k (x :: HMKI) ->
          let f = const (Identity Nothing)
              y = HM.alterF f k x
          in  fmap toOrdMap y === M.alterF f k (toOrdMap x)
      , testProperty "lookup" $
        \(f :: Fun (Maybe A) B) k (x :: HMK A) ->
          let g = Const . apply f
              y = HM.alterF g k x
          in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
      ]
    , testGroup "isSubmapOf"
      [ testProperty "model" $
        \(x :: HMKI) y -> HM.isSubmapOf x y === M.isSubmapOf (toOrdMap x) (toOrdMap y)
      , testProperty "m ⊆ m" $
        \(x :: HMKI) -> HM.isSubmapOf x x
      , testProperty "m1 ⊆ m1 ∪ m2" $
        \(x :: HMKI) y -> HM.isSubmapOf x (HM.union x y)
      , testProperty "m1 ⊈ m2  ⇒  m1 ∪ m2 ⊈ m1" $
        \(m1 :: HMKI) m2 -> not (HM.isSubmapOf m1 m2) ==> HM.isSubmapOf m1 (HM.union m1 m2)
      , testProperty "m1\\m2 ⊆ m1" $
        \(m1 :: HMKI) (m2 :: HMKI) -> HM.isSubmapOf (HM.difference m1 m2) m1
      , testProperty "m1 ∩ m2 ≠ ∅  ⇒  m1 ⊈ m1\\m2 " $
        \(m1 :: HMKI) (m2 :: HMKI) ->
          not (HM.null (HM.intersection m1 m2)) ==>
          not (HM.isSubmapOf m1 (HM.difference m1 m2))
      , testProperty "delete k m ⊆ m" $
        \(m :: HMKI) ->
          not (HM.null m) ==>
          forAll (elements (HM.keys m)) $ \k ->
          HM.isSubmapOf (HM.delete k m) m
      , testProperty "m ⊈ delete k m " $
        \(m :: HMKI) ->
          not (HM.null m) ==>
          forAll (elements (HM.keys m)) $ \k ->
          not (HM.isSubmapOf m (HM.delete k m))
      , testProperty "k ∉ m  ⇒  m ⊆ insert k v m" $
        \k v (m :: HMKI) -> not (HM.member k m) ==> HM.isSubmapOf m (HM.insert k v m)
      , testProperty "k ∉ m  ⇒  insert k v m ⊈ m" $
        \k v (m :: HMKI) -> not (HM.member k m) ==> not (HM.isSubmapOf (HM.insert k v m) m)
      ]
    -- Combine
    , testProperty "union" $
      \(x :: HMKI) y ->
        let z = HM.union x y
        in  toOrdMap z === M.union (toOrdMap x) (toOrdMap y)
    , testProperty "unionWith" $
      \f (x :: HMKI) y ->
        let z = HM.unionWith (applyFun2 f) x y
        in  toOrdMap z === M.unionWith (applyFun2 f) (toOrdMap x) (toOrdMap y)
    , testProperty "unionWithKey" $
      \f (x :: HMKI) y ->
        let z = HM.unionWithKey (applyFun3 f) x y
        in  toOrdMap z === M.unionWithKey (applyFun3 f) (toOrdMap x) (toOrdMap y)
    , testProperty "unions" $
      \(ms :: [HMKI]) -> toOrdMap (HM.unions ms) === M.unions (map toOrdMap ms)
    -- Transformations
    , testProperty "map" $
      \(f :: Fun A B) (m :: HMK A) -> toOrdMap (HM.map (apply f) m) === M.map (apply f) (toOrdMap m) 
    , testProperty "traverseWithKey" $ QC.mapSize (\s -> s `div` 8) $
      \(x :: HMKI) ->
        let f k v = [keyToInt k + v + 1, keyToInt k + v + 2]
            y = HM.traverseWithKey f x
        in  List.sort (fmap toOrdMap y) === List.sort (M.traverseWithKey f (toOrdMap x))
    , testProperty "mapKeys" $
      \(m :: HMKI) -> toOrdMap (HM.mapKeys incKey m) === M.mapKeys incKey (toOrdMap m)
    -- Folds
    , testProperty "foldr" $
      \(m :: HMKI) -> List.sort (HM.foldr (:) [] m) === List.sort (M.foldr (:) [] (toOrdMap m))
    , testProperty "foldl" $
      \(m :: HMKI) ->
        List.sort (HM.foldl (flip (:)) [] m) === List.sort (M.foldl (flip (:)) [] (toOrdMap m))
    , testProperty "foldrWithKey" $
      \(m :: HMKI) ->
        let f k v z = (k, v) : z
        in  sortByKey (HM.foldrWithKey f [] m) === sortByKey (M.foldrWithKey f [] (toOrdMap m))
    , testProperty "foldlWithKey" $
      \(m :: HMKI) ->
        let f z k v = (k, v) : z
        in  sortByKey (HM.foldlWithKey f [] m) === sortByKey (M.foldlWithKey f [] (toOrdMap m))
    , testProperty "foldrWithKey'" $
      \(m :: HMKI) ->
        let f k v z = (k, v) : z
        in  sortByKey (HM.foldrWithKey' f [] m) === sortByKey (M.foldrWithKey' f [] (toOrdMap m))
    , testProperty "foldlWithKey'" $
      \(m :: HMKI) ->
        let f z k v = (k, v) : z
        in  sortByKey (HM.foldlWithKey' f [] m) === sortByKey (M.foldlWithKey' f [] (toOrdMap m))
    , testProperty "foldl'" $
      \(m :: HMKI) ->
        List.sort (HM.foldl' (flip (:)) [] m) === List.sort (M.foldl' (flip (:)) [] (toOrdMap m))
    , testProperty "foldr'" $
      \(m :: HMKI) -> List.sort (HM.foldr' (:) [] m) === List.sort (M.foldr' (:) [] (toOrdMap m))
    , testProperty "foldMapWithKey" $
      \(m :: HMKI) ->
        let f k v = [(k, v)]
        in  sortByKey (HM.foldMapWithKey f m) === sortByKey (M.foldMapWithKey f (toOrdMap m))
    , testProperty "difference" $
      \(x :: HMKI) (y :: HMKI) ->
        toOrdMap (HM.difference x y) === M.difference (toOrdMap x) (toOrdMap y)
    , testProperty "differenceWith" $
      \f (x :: HMK A) (y :: HMK B) ->
        toOrdMap (HM.differenceWith (applyFun2 f) x y) === M.differenceWith (applyFun2 f) (toOrdMap x) (toOrdMap y)
    , testGroup "intersection"
      [ testProperty "model" $
        \(x :: HMKI) (y :: HMKI) ->
          toOrdMap (HM.intersection x y) === M.intersection (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) (y :: HMKI) ->
          isValid (HM.intersection x y)
      ]
    , testProperty "intersectionWith" $
      \(f :: Fun (A, B) C) (x :: HMK A) (y :: HMK B) ->
        toOrdMap (HM.intersectionWith (applyFun2 f) x y) === M.intersectionWith (applyFun2 f) (toOrdMap x) (toOrdMap y)
    , testProperty "intersectionWithKey" $
      \(f :: Fun (Key, A, B) C) (x :: HMK A) (y :: HMK B) ->
        toOrdMap (HM.intersectionWithKey (applyFun3 f) x y) === M.intersectionWithKey (applyFun3 f) (toOrdMap x) (toOrdMap y)
    -- Filter
    , testProperty "filter" $
      \p (m :: HMKI) ->
        toOrdMap (HM.filter (apply p) m) === M.filter (apply p) (toOrdMap m)
    , testProperty "filterWithKey" $
      \p (m :: HMKI) ->
        toOrdMap (HM.filterWithKey (applyFun2 p) m) === M.filterWithKey (applyFun2 p) (toOrdMap m)
    , testProperty "mapMaybe" $
      \(f :: Fun A (Maybe B)) (m :: HMK A) ->
        toOrdMap (HM.mapMaybe (apply f) m) === M.mapMaybe (apply f) (toOrdMap m)
    , testProperty "mapMaybeWithKey" $
      \(f :: Fun (Key, A) (Maybe B)) (m :: HMK A) ->
        toOrdMap (HM.mapMaybeWithKey (applyFun2 f) m) === M.mapMaybeWithKey (applyFun2 f) (toOrdMap m)
{-
-- 'eq_' already calls fromList.
pFromList :: [(Key, Int)] -> Property
pFromList = id `eq_` id

pFromListValid :: [(Key, ())] -> Property
pFromListValid xs = valid (HM.fromList xs) === Valid

pFromListWith :: [(Key, Int)] -> Property
pFromListWith kvs = (M.toAscList $ M.fromListWith Op kvsM) ===
                    (toAscList $ HM.fromListWith Op kvsM)
  where kvsM = fmap (fmap Leaf) kvs

pFromListWithKey :: [(Key, Int)] -> Property
pFromListWithKey kvs = (M.toAscList $ M.fromListWithKey combine kvsM) ===
                       (toAscList $ HM.fromListWithKey combine kvsM)
  where kvsM = fmap (\(k,v) -> (Leaf (keyToInt k), Leaf v)) kvs
        combine k v1 v2 = Op k (Op v1 v2)

pToList :: [(Key, Int)] -> Property
pToList = M.toAscList `eq` toAscList

pElems :: [(Key, Int)] -> Property
pElems = (List.sort . M.elems) `eq` (List.sort . HM.elems)

pKeys :: [(Key, Int)] -> Property
pKeys = (List.sort . M.keys) `eq` (List.sort . HM.keys)
-}
    -- Conversions
    , testGroup "conversions"
      [ testProperty "elems" pElems
      , testProperty "keys" pKeys
      , testProperty "fromList" pFromList
      , testProperty "fromList.valid" pFromListValid
      , testProperty "fromListWith" pFromListWith
      , testProperty "fromListWithKey" pFromListWithKey
      , testProperty "toList" pToList
      ]
    ]

------------------------------------------------------------------------
-- * Model

type Model k v = M.Map k v

-- | Check that a function operating on a 'HashMap' is equivalent to
-- one operating on a 'Model'.
eq :: (Eq a, Eq k, Hashable k, Ord k, Show a, Show k)
   => (Model k v -> a)       -- ^ Function that modifies a 'Model'
   -> (HashMap k v -> a)     -- ^ Function that modified a 'HashMap' in the same
                             -- way
   -> [(k, v)]               -- ^ Initial content of the 'HashMap' and 'Model'
   -> Property
eq f g xs = f (M.fromList xs) === g (HM.fromList xs)

infix 4 `eq`

eq_ :: (Eq k, Eq v, Hashable k, Ord k, Show k, Show v)
    => (Model k v -> Model k v)            -- ^ Function that modifies a 'Model'
    -> (HashMap k v -> HashMap k v)        -- ^ Function that modified a
                                           -- 'HashMap' in the same way
    -> [(k, v)]                            -- ^ Initial content of the 'HashMap'
                                           -- and 'Model'
    -> Property
eq_ f g = (M.toAscList . f) `eq` (toAscList . g)

infix 4 `eq_`

------------------------------------------------------------------------
-- * Helpers

type HMK  = HashMap Key
type HMKI = HMK Int

sortByKey :: Ord k => [(k, v)] -> [(k, v)]
sortByKey = List.sortBy (compare `on` fst)

toAscList :: Ord k => HashMap k v -> [(k, v)]
toAscList = List.sortBy (compare `on` fst) . HM.toList

toOrdMap :: Ord k => HashMap k v -> M.Map k v
toOrdMap = M.fromList . HM.toList

isValid :: (Eq k, Hashable k, Show k) => HashMap k v -> Property
isValid m = valid m === Valid
