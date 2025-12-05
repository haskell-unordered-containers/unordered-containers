{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-orphans            #-} -- because of Arbitrary (HashMap k v)
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- https://github.com/nick8325/quickcheck/issues/344

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
import Data.Maybe                  (isJust)
import Data.Ord                    (comparing)
import Test.QuickCheck             (Arbitrary (..), Fun, Property, pattern Fn,
                                    pattern Fn2, pattern Fn3, (===), (==>))
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

instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
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

isValid :: (Hashable k, Show k) => HashMap k v -> Property
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
        \(x :: HMKI) (Fn f :: Fun Int Int) ->
          toOrdMap (fmap f x) === fmap f (toOrdMap x)
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
            bifoldl (flip (:)) (flip (:)) [] m
            ===
            reverse (concatMap (\(k, v) -> [k, v]) (HM.toList m))
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
    , testGroup "lookupKey" $
      [ testProperty "isJust (lookupKey k m) == member k m" $
        \(k :: Key) (m :: HMKI) -> isJust (HM.lookupKey k m) === HM.member k m
      ]
    , testGroup "insert"
      [ testProperty "model" $
        \(k :: Key) (v :: Int) x ->
          let y = HM.insert k v x
          in  toOrdMap y === M.insert k v (toOrdMap x)
      , testProperty "valid" $
        \(k :: Key) (v :: Int) x -> isValid (HM.insert k v x)
      ]
    , testGroup "insertWith"
      [ testProperty "model" $
        \(Fn2 f) k v (x :: HMKI) ->
          toOrdMap (HM.insertWith f k v x) === M.insertWith f k v (toOrdMap x)
      , testProperty "valid" $
        \(Fn2 f) k v (x :: HMKI) -> isValid (HM.insertWith f k v x)
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
        \(Fn f) k (x :: HMKI) ->
          toOrdMap (HM.adjust f k x) === M.adjust f k (toOrdMap x)
      , testProperty "valid" $
        \(Fn f) k (x :: HMKI) -> isValid (HM.adjust f k x)
      ]
    , testGroup "update" 
      [ testProperty "model" $
        \(Fn f) k (x :: HMKI) ->
          toOrdMap (HM.update f k x) === M.update f k (toOrdMap x)
      , testProperty "valid" $
        \(Fn f) k (x :: HMKI) -> isValid (HM.update f k x)
      ]
    , testGroup "alter"
      [ testProperty "model" $
        \(Fn f) k (x :: HMKI) ->
          toOrdMap (HM.alter f k x) === M.alter f k (toOrdMap x)
      , testProperty "valid" $
        \(Fn f) k (x :: HMKI) -> isValid (HM.alter f k x)
      ]
    , testGroup "alterF"
      [ testGroup "model"
        [ -- We choose the list functor here because we don't fuss with
          -- it in alterF rules and because it has a sufficiently interesting
          -- structure to have a good chance of breaking if something is wrong.
          testProperty "[]" $
          \(Fn f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
            map toOrdMap (HM.alterF f k x) === M.alterF f k (toOrdMap x)
        , testProperty "adjust" $
          \(Fn f) k (x :: HMKI) ->
            let g = Identity . fmap f
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        , testProperty "insert" $
          \v k (x :: HMKI) ->
            let g = const . Identity . Just $ v
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        , testProperty "insertWith" $
          \(Fn f) k v (x :: HMKI) ->
            let g = Identity . Just . maybe v f
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        , testProperty "delete" $
          \k (x :: HMKI) ->
            let f = const (Identity Nothing)
            in  fmap toOrdMap (HM.alterF f k x) === M.alterF f k (toOrdMap x)
        , testProperty "lookup" $
          \(Fn f :: Fun (Maybe A) B) k (x :: HMK A) ->
            let g = Const . f
            in  fmap toOrdMap (HM.alterF g k x) === M.alterF g k (toOrdMap x)
        ]
      , testProperty "valid" $
        \(Fn f :: Fun (Maybe A) [Maybe A]) k (x :: HMK A) ->
          let ys = HM.alterF f k x
          in  map valid ys === (Valid <$ ys)
      ]
    , testGroup "isSubmapOf"
      [ testProperty "model" $
        \(x :: HMKI) y -> HM.isSubmapOf x y === M.isSubmapOf (toOrdMap x) (toOrdMap y)
      , testProperty "m ⊆ m" $
        \(x :: HMKI) -> HM.isSubmapOf x x
      , testProperty "delete k m ⊆ m" $
        \k (m :: HMKI) -> HM.isSubmapOf (HM.delete k m) m
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
    , testGroup "union"
      [ testProperty "model" $
        \(x :: HMKI) y ->
          let z = HM.union x y
          in  toOrdMap z === M.union (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) y -> isValid (HM.union x y)
      ]
    , testGroup "unionWith"
      [ testProperty "model" $
        \(Fn2 f) (x :: HMKI) y ->
          toOrdMap (HM.unionWith f x y) === M.unionWith f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn2 f) (x :: HMKI) y -> isValid (HM.unionWith f x y)
      ]
    , testGroup "unionWithKey"
      [ testProperty "model" $
        \(Fn3 f) (x :: HMKI) y ->
          toOrdMap (HM.unionWithKey f x y) === M.unionWithKey f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn3 f) (x :: HMKI) y -> isValid (HM.unionWithKey f x y)
      ]
    , testGroup "unions"
      [ testProperty "model" $
        \(ms :: [HMKI]) -> toOrdMap (HM.unions ms) === M.unions (map toOrdMap ms)
      , testProperty "valid" $
        \(ms :: [HMKI]) -> isValid (HM.unions ms)
      ]
    , testGroup "difference"
      [ testProperty "model" $
        \(x :: HMKI) (y :: HMKI) ->
          toOrdMap (HM.difference x y) === M.difference (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) (y :: HMKI) -> isValid (HM.difference x y)
      ]
    , testGroup "differenceWith"
      [ testProperty "model" $
        \(Fn2 f) (x :: HMK A) (y :: HMK B) ->
          toOrdMap (HM.differenceWith f x y) === M.differenceWith f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn2 f) (x :: HMK A) (y :: HMK B) -> isValid (HM.differenceWith f x y)
      , testProperty "differenceWith (\\x y -> Just $ f x y) xs ys == intersectionWith f xs ys `union` xs" $
        \(Fn2 f) (x :: HMK A) (y :: HMK B) ->
          HM.differenceWith (\a b -> Just $ f a b) x y
          === HM.intersectionWith f x y `HM.union` x
      ]
    , testGroup "differenceWithKey"
      [ testProperty "model" $
        \(Fn3 f) (x :: HMK A) (y :: HMK B) ->
          toOrdMap (HM.differenceWithKey f x y) === M.differenceWithKey f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn3 f) (x :: HMK A) (y :: HMK B) -> isValid (HM.differenceWithKey f x y)
      , testProperty "differenceWithKey (\\k x y -> Just $ f k x y) xs ys == intersectionWithKey f xs ys `union` xs" $
        \(Fn3 f) (x :: HMK A) (y :: HMK B) ->
          HM.differenceWithKey (\k a b -> Just $ f k a b) x y
          === HM.intersectionWithKey f x y `HM.union` x
      ]
    , testGroup "intersection"
      [ testProperty "model" $
        \(x :: HMKI) (y :: HMKI) ->
          toOrdMap (HM.intersection x y) === M.intersection (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(x :: HMKI) (y :: HMKI) ->
          isValid (HM.intersection x y)
      ]
    , testGroup "intersectionWith"
      [ testProperty "model" $
        \(Fn2 f :: Fun (A, B) C) (x :: HMK A) (y :: HMK B) ->
          toOrdMap (HM.intersectionWith f x y) === M.intersectionWith f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn2 f :: Fun (A, B) C) (x :: HMK A) (y :: HMK B) ->
          isValid (HM.intersectionWith f x y)
      ]
    , testGroup "intersectionWithKey"
      [ testProperty "model" $
        \(Fn3 f :: Fun (Key, A, B) C) (x :: HMK A) (y :: HMK B) ->
          toOrdMap (HM.intersectionWithKey f x y)
          ===
          M.intersectionWithKey f (toOrdMap x) (toOrdMap y)
      , testProperty "valid" $
        \(Fn3 f :: Fun (Key, A, B) C) (x :: HMK A) (y :: HMK B) ->
          isValid (HM.intersectionWithKey f x y)
      ]
    , testGroup "disjoint"
      [ testProperty "model" $
        \(x :: HMKI) (y :: HMKI) ->
          HM.disjoint x y === M.disjoint (toOrdMap x) (toOrdMap y)
      ]
    , testGroup "compose"
      [ testProperty "valid" $
        \(x :: HMK Int) (y :: HMK Key) -> isValid (HM.compose x y)
      ]
    -- Transformations
    , testGroup "map"
      [ testProperty "model" $
        \(Fn f :: Fun A B) (m :: HMK A) -> toOrdMap (HM.map f m) === M.map f (toOrdMap m)
      , testProperty "valid" $
        \(Fn f :: Fun A B) (m :: HMK A) -> isValid (HM.map f m)
      ]
    , testGroup "traverseWithKey"
      [ testProperty "model" $ QC.mapSize (\s -> min 18 $ div s 8) $
        \(x :: HMKI) ->
          let f k v = [keyToInt k + v + 1, keyToInt k + v + 2]
              ys = HM.traverseWithKey f x
          in  List.sort (fmap toOrdMap ys) === List.sort (M.traverseWithKey f (toOrdMap x))
      , testProperty "valid" $ QC.mapSize (\s -> min 18 $ div s 8) $
        \(x :: HMKI) ->
          let f k v = [keyToInt k + v + 1, keyToInt k + v + 2]
              ys = HM.traverseWithKey f x
          in  fmap valid ys === (Valid <$ ys)
      ]
    , testGroup "mapKeys"
      [ testProperty "model" $
        \(m :: HMKI) -> toOrdMap (HM.mapKeys incKey m) === M.mapKeys incKey (toOrdMap m)
      , testProperty "valid" $
        \(Fn f :: Fun Key Key) (m :: HMKI) -> isValid (HM.mapKeys f m)
      ]
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
    , testGroup "filter"
      [ testProperty "model" $
        \(Fn p) (m :: HMKI) -> toOrdMap (HM.filter p m) === M.filter p (toOrdMap m)
      , testProperty "valid" $
        \(Fn p) (m :: HMKI) -> isValid (HM.filter p m)
      ]
    , testGroup "filterWithKey"
      [ testProperty "model" $
        \(Fn2 p) (m :: HMKI) ->
          toOrdMap (HM.filterWithKey p m) === M.filterWithKey p (toOrdMap m)
      , testProperty "valid" $
        \(Fn2 p) (m :: HMKI) -> isValid (HM.filterWithKey p m)
      ]
    , testGroup "mapMaybe"
      [ testProperty "model" $
        \(Fn f :: Fun A (Maybe B)) (m :: HMK A) ->
          toOrdMap (HM.mapMaybe f m) === M.mapMaybe f (toOrdMap m)
      , testProperty "valid" $
        \(Fn f :: Fun A (Maybe B)) (m :: HMK A) -> isValid (HM.mapMaybe f m)
      ]
    , testGroup "mapMaybeWithKey"
      [ testProperty "model" $
        \(Fn2 f :: Fun (Key, A) (Maybe B)) (m :: HMK A) ->
          toOrdMap (HM.mapMaybeWithKey f m) === M.mapMaybeWithKey f (toOrdMap m)
      , testProperty "valid" $
        \(Fn2 f :: Fun (Key, A) (Maybe B)) (m :: HMK A) ->
          isValid (HM.mapMaybeWithKey f m)
      ]
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
    , testGroup "fromListWith"
      [ testProperty "model" $
        \(kvs :: [(Key, Int)]) ->
          let kvsM = map (fmap Leaf) kvs
          in  toOrdMap (HM.fromListWith Op kvsM) === M.fromListWith Op kvsM
      , testProperty "valid" $
        \(Fn2 f) (kvs :: [(Key, A)]) -> isValid (HM.fromListWith f kvs)
      ]
    , testGroup "fromListWithKey"
      [ testProperty "model" $
        \(kvs :: [(Key, Int)]) ->
          let kvsM = fmap (\(k,v) -> (Leaf (keyToInt k), Leaf v)) kvs
              combine k v1 v2 = Op k (Op v1 v2)
          in  toOrdMap (HM.fromListWithKey combine kvsM) === M.fromListWithKey combine kvsM
      , testProperty "valid" $
        \(Fn3 f) (kvs :: [(Key, A)]) -> isValid (HM.fromListWithKey f kvs)
      ]
    , testProperty "toList" $
      \(m :: HMKI) -> List.sort (HM.toList m) === List.sort (M.toList (toOrdMap m))
    ]
