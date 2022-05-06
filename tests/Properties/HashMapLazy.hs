{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- because of Arbitrary (HashMap k v)

-- | Tests for "Data.HashMap.Lazy" and "Data.HashMap.Strict".  We test functions by
-- comparing them to @Map@ from @containers@. @Map@ is referred to as the /model/
-- for 'HashMap'

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
import Test.QuickCheck             (Arbitrary (..), Fun, Property, (===), (==>))
import Test.QuickCheck.Poly        (A, B, C)
import Test.Tasty                  (TestTree, testGroup)
import Test.Tasty.QuickCheck       (testProperty)
import Util.Key                    (Key, incKey, keyToInt)

import qualified Data.Foldable   as Foldable
import qualified Data.List       as List
import qualified Test.QuickCheck as QC

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
-- Helpers

type HMK  = HashMap Key
type HMKI = HMK Int

sortByKey :: Ord k => [(k, v)] -> [(k, v)]
sortByKey = List.sortBy (compare `on` fst)

toOrdMap :: Ord k => HashMap k v -> M.Map k v
toOrdMap = M.fromList . HM.toList

isValid :: (Eq k, Hashable k, Show k) => HashMap k v -> Property
isValid m = valid m === Valid

-- The free magma is used to test that operations are applied in the
-- same order.
data Magma a
  = Leaf a
  | Op (Magma a) (Magma a)
  deriving (Show, Eq, Ord)

instance Hashable a => Hashable (Magma a) where
  hashWithSalt s (Leaf a) = hashWithSalt s (hashWithSalt (1::Int) a)
  hashWithSalt s (Op m n) = hashWithSalt s (hashWithSalt (hashWithSalt (2::Int) m) n)

------------------------------------------------------------------------
-- Test list

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
            (LT, GT) -> QC.property True -- ys greater than xs and zs.
            (GT, LT) -> QC.property True
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
          let y = fmap (QC.applyFun f) x
          in  toOrdMap y === fmap (QC.applyFun f) (toOrdMap x)
      , testProperty "Foldable" $
        \(x :: HMKI) ->
          let f = List.sort . Foldable.foldr (:) []
          in  f x === f (toOrdMap x)
      , testGroup "Bifoldable"
        [ testProperty "bifoldMap" $
          \(m :: HMK Key) ->
            bifoldMap (:[]) (:[]) m === concatMap (\(k, v) -> [k, v]) (HM.toList m)
        , testProperty "bifoldr" $
          \(m :: HMK Key) ->
            bifoldr (:) (:) [] m === concatMap (\(k, v) -> [k, v]) (HM.toList m)
        , testProperty "bifoldl" $
          \(m :: HMK Key) ->
            bifoldl (flip (:)) (flip (:)) [] m === reverse (concatMap (\(k, v) -> [k, v]) (HM.toList m))
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
    -- Construction
    , testGroup "empty"
      [ testProperty "valid" $ QC.once $
        isValid (HM.empty :: HMKI)
      ]
    , testGroup "singleton"
      [ testProperty "valid" $
        \(k :: Key) (v :: A) -> isValid (HM.singleton k v)
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
    , testGroup "insert"
      [ testProperty "model" $
        \(k :: Key) (v :: Int) x ->
          let y = HM.insert k v x
          in  toOrdMap y === M.insert k v (toOrdMap x)
      , testProperty "valid" $
        \(k :: Key) (v :: Int) x -> isValid (HM.insert k v x)
      ]
    , testGroup "insertWith"
      [ testProperty "insertWith" $
        \f k v (x :: HMKI) ->
          let y = HM.insertWith (QC.applyFun2 f) k v x
          in  toOrdMap y === M.insertWith (QC.applyFun2 f) k v (toOrdMap x)
      , testProperty "valid" $
        \f k v (x :: HMKI) -> isValid (HM.insertWith (QC.applyFun2 f) k v x)
      ]
    , testGroup "delete"
      [ testProperty "model" $
        \(k :: Key) (x :: HMKI) ->
          let y = HM.delete k x
          in  toOrdMap y === M.delete k (toOrdMap x)
      , testProperty "valid" $
        \(k :: Key) (x :: HMKI) -> isValid (HM.delete k x)
      ]
    , testGroup "adjust" 
      [ testProperty "model" $
        \f k (x :: HMKI) ->
          let y = HM.adjust (QC.applyFun f) k x
          in  toOrdMap y === M.adjust (QC.applyFun f) k (toOrdMap x)
      , testProperty "valid" $
        \f k (x :: HMKI) -> isValid (HM.adjust (QC.applyFun f) k x)
      ]
    , testGroup "update" 
      [ testProperty "model" $
        \f k (x :: HMKI) ->
          let y = HM.update (QC.applyFun f) k x
          in  toOrdMap y === M.update (QC.applyFun f) k (toOrdMap x)
      , testProperty "valid" $
        \f k (x :: HMKI) -> isValid (HM.update (QC.applyFun f) k x)
      ]
    , testGroup "alter"
      [ testProperty "model" $
        \f k (x :: HMKI) ->
          let y = HM.alter (QC.applyFun f) k x
          in  toOrdMap y === M.alter (QC.applyFun f) k (toOrdMap x)
      , testProperty "valid" $
        \f k (x :: HMKI) -> isValid (HM.alter (QC.applyFun f) k x)
      ]
    , testGroup "alterF"
      [ testGroup "model"
        [ -- We choose the list functor here because we don't fuss with
          -- it in alterF rules and because it has a sufficiently interesting
          -- structure to have a good chance of breaking if something is wrong.
          testProperty "[]" $
          \(f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
            let ys = HM.alterF (QC.applyFun f) k x
            in  map toOrdMap ys === M.alterF (QC.applyFun f) k (toOrdMap x)
        , testProperty "adjust" $
          \f k (x :: HMKI) ->
            let g = Identity . fmap (QC.applyFun f)
                y = HM.alterF g k x
            in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
        , testProperty "insert" $
          \v k (x :: HMKI) ->
            let g = const . Identity . Just $ v
                y = HM.alterF g k x
            in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
        , testProperty "insertWith" $
          \f k v (x :: HMKI) ->
            let g = Identity . Just . maybe v (QC.applyFun f)
                y = HM.alterF g k x
            in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
        , testProperty "delete" $
          \k (x :: HMKI) ->
            let f = const (Identity Nothing)
                y = HM.alterF f k x
            in  fmap toOrdMap y === M.alterF f k (toOrdMap x)
        , testProperty "lookup" $
          \(f :: Fun (Maybe A) B) k (x :: HMK A) ->
            let g = Const . QC.applyFun f
                y = HM.alterF g k x
            in  fmap toOrdMap y === M.alterF g k (toOrdMap x)
        ]
      , testProperty "valid" $
        \(f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
          let ys = HM.alterF (QC.applyFun f) k x
          in  map valid ys === (Valid <$ ys)
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
          QC.forAll (QC.elements (HM.keys m)) $ \k ->
          HM.isSubmapOf (HM.delete k m) m
      , testProperty "m ⊈ delete k m " $
        \(m :: HMKI) ->
          not (HM.null m) ==>
          QC.forAll (QC.elements (HM.keys m)) $ \k ->
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
        let z = HM.unionWith (QC.applyFun2 f) x y
        in  toOrdMap z === M.unionWith (QC.applyFun2 f) (toOrdMap x) (toOrdMap y)
    , testProperty "unionWithKey" $
      \f (x :: HMKI) y ->
        let z = HM.unionWithKey (QC.applyFun3 f) x y
        in  toOrdMap z === M.unionWithKey (QC.applyFun3 f) (toOrdMap x) (toOrdMap y)
    , testProperty "unions" $
      \(ms :: [HMKI]) -> toOrdMap (HM.unions ms) === M.unions (map toOrdMap ms)
    , testProperty "difference" $
      \(x :: HMKI) (y :: HMKI) ->
        toOrdMap (HM.difference x y) === M.difference (toOrdMap x) (toOrdMap y)
    , testProperty "differenceWith" $
      \f (x :: HMK A) (y :: HMK B) ->
        toOrdMap (HM.differenceWith (QC.applyFun2 f) x y)
        ===
        M.differenceWith (QC.applyFun2 f) (toOrdMap x) (toOrdMap y)
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
        toOrdMap (HM.intersectionWith (QC.applyFun2 f) x y)
        ===
        M.intersectionWith (QC.applyFun2 f) (toOrdMap x) (toOrdMap y)
    , testProperty "intersectionWithKey" $
      \(f :: Fun (Key, A, B) C) (x :: HMK A) (y :: HMK B) ->
        toOrdMap (HM.intersectionWithKey (QC.applyFun3 f) x y)
        ===
        M.intersectionWithKey (QC.applyFun3 f) (toOrdMap x) (toOrdMap y)
    -- Transformations
    , testProperty "map" $
      \(f :: Fun A B) (m :: HMK A) -> toOrdMap (HM.map (QC.applyFun f) m) === M.map (QC.applyFun f) (toOrdMap m)
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
    -- Filter
    , testProperty "filter" $
      \p (m :: HMKI) ->
        toOrdMap (HM.filter (QC.applyFun p) m) === M.filter (QC.applyFun p) (toOrdMap m)
    , testProperty "filterWithKey" $
      \p (m :: HMKI) ->
        toOrdMap (HM.filterWithKey (QC.applyFun2 p) m) === M.filterWithKey (QC.applyFun2 p) (toOrdMap m)
    , testProperty "mapMaybe" $
      \(f :: Fun A (Maybe B)) (m :: HMK A) ->
        toOrdMap (HM.mapMaybe (QC.applyFun f) m) === M.mapMaybe (QC.applyFun f) (toOrdMap m)
    , testProperty "mapMaybeWithKey" $
      \(f :: Fun (Key, A) (Maybe B)) (m :: HMK A) ->
        toOrdMap (HM.mapMaybeWithKey (QC.applyFun2 f) m) === M.mapMaybeWithKey (QC.applyFun2 f) (toOrdMap m)
    -- Conversions
    , testProperty "elems" $
      \(m :: HMKI) -> List.sort (HM.elems m) === List.sort (M.elems (toOrdMap m))
    , testProperty "keys" $
      \(m :: HMKI) -> List.sort (HM.keys m) === List.sort (M.keys (toOrdMap m))
    , testGroup "fromList"
      [ testProperty "model" $
        \(kvs :: [(Key, Int)]) -> toOrdMap (HM.fromList kvs) === M.fromList kvs
      , testProperty "valid" $
        \(kvs :: [(Key, Int)]) -> isValid (HM.fromList kvs)
      ]
    , testProperty "fromListWith" $
      \(kvs :: [(Key, Int)]) ->
        let kvsM = map (fmap Leaf) kvs
        in  toOrdMap (HM.fromListWith Op kvsM) === M.fromListWith Op kvsM
    , testProperty "fromListWithKey" $
      \(kvs :: [(Key, Int)]) ->
        let kvsM = fmap (\(k,v) -> (Leaf (keyToInt k), Leaf v)) kvs
            combine k v1 v2 = Op k (Op v1 v2)
        in  toOrdMap (HM.fromListWithKey combine kvsM) === M.fromListWithKey combine kvsM
    , testProperty "toList" $
      \(m :: HMKI) -> List.sort (HM.toList m) === List.sort (M.toList (toOrdMap m))
    ]
