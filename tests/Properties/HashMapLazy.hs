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
import Control.Monad               (guard)
import Data.Bifoldable
import Data.Function               (on)
import Data.Functor.Identity       (Identity (..))
import Data.Hashable               (Hashable (hashWithSalt))
import Data.HashMap.Internal.Debug (Validity (..), valid)
import Data.Ord                    (comparing)
import Test.QuickCheck             (Arbitrary (..), Property, elements, forAll,
                                    property, (===), (==>))
import Test.QuickCheck.Function    (Fun, apply, applyFun2, applyFun3)
import Test.QuickCheck.Poly        (A, B)
import Test.Tasty                  (TestTree, testGroup)
import Test.Tasty.QuickCheck       (testProperty)
import Util.Key                    (Key, keyToInt)

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
-- ** Transformations

pMap :: [(Key, Int)] -> Property
pMap = M.map (+ 1) `eq_` HM.map (+ 1)

pTraverse :: [(Key, Int)] -> Property
pTraverse xs =
  List.sort (fmap (List.sort . M.toList) (M.traverseWithKey (\_ v -> [v + 1, v + 2]) (M.fromList (take 10 xs))))
     === List.sort (fmap (List.sort . HM.toList) (HM.traverseWithKey (\_ v -> [v + 1, v + 2]) (HM.fromList (take 10 xs))))

pMapKeys :: [(Int, Int)] -> Property
pMapKeys = M.mapKeys (+1) `eq_` HM.mapKeys (+1)

------------------------------------------------------------------------
-- ** Difference and intersection

pDifference :: [(Key, Int)] -> [(Key, Int)] -> Property
pDifference xs ys = M.difference (M.fromList xs) `eq_`
                    HM.difference (HM.fromList xs) $ ys

pDifferenceWith :: [(Key, Int)] -> [(Key, Int)] -> Property
pDifferenceWith xs ys = M.differenceWith f (M.fromList xs) `eq_`
                        HM.differenceWith f (HM.fromList xs) $ ys
  where
    f x y = if x == 0 then Nothing else Just (x - y)

pIntersection :: [(Key, Int)] -> [(Key, Int)] -> Property
pIntersection xs ys = 
  M.intersection (M.fromList xs)
    `eq_` HM.intersection (HM.fromList xs)
    $ ys

pIntersectionValid :: HashMap Key () -> HashMap Key () -> Property
pIntersectionValid x y = valid (HM.intersection x y) === Valid

pIntersectionWith :: [(Key, Int)] -> [(Key, Int)] -> Property
pIntersectionWith xs ys = M.intersectionWith (-) (M.fromList xs) `eq_`
                          HM.intersectionWith (-) (HM.fromList xs) $ ys

pIntersectionWithKey :: [(Key, Int)] -> [(Key, Int)] -> Property
pIntersectionWithKey xs ys = M.intersectionWithKey go (M.fromList xs) `eq_`
                             HM.intersectionWithKey go (HM.fromList xs) $ ys
  where
    go :: Key -> Int -> Int -> Int
    go k i1 i2 = keyToInt k - i1 - i2

------------------------------------------------------------------------
-- ** Folds

pFoldr :: [(Int, Int)] -> Property
pFoldr = (List.sort . M.foldr (:) []) `eq` (List.sort . HM.foldr (:) [])

pFoldl :: [(Int, Int)] -> Property
pFoldl = (List.sort . M.foldl (flip (:)) []) `eq` (List.sort . HM.foldl (flip (:)) [])

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

pFoldrWithKey :: [(Int, Int)] -> Property
pFoldrWithKey = (sortByKey . M.foldrWithKey f []) `eq`
                (sortByKey . HM.foldrWithKey f [])
  where f k v z = (k, v) : z

pFoldMapWithKey :: [(Int, Int)] -> Property
pFoldMapWithKey = (sortByKey . M.foldMapWithKey f) `eq`
                  (sortByKey . HM.foldMapWithKey f)
  where f k v = [(k, v)]

pFoldrWithKey' :: [(Int, Int)] -> Property
pFoldrWithKey' = (sortByKey . M.foldrWithKey' f []) `eq`
                 (sortByKey . HM.foldrWithKey' f [])
  where f k v z = (k, v) : z

pFoldlWithKey :: [(Int, Int)] -> Property
pFoldlWithKey = (sortByKey . M.foldlWithKey f []) `eq`
                (sortByKey . HM.foldlWithKey f [])
  where f z k v = (k, v) : z

pFoldlWithKey' :: [(Int, Int)] -> Property
pFoldlWithKey' = (sortByKey . M.foldlWithKey' f []) `eq`
                 (sortByKey . HM.foldlWithKey' f [])
  where f z k v = (k, v) : z

pFoldl' :: [(Int, Int)] -> Property
pFoldl' = (List.sort . M.foldl' (flip (:)) []) `eq` (List.sort . HM.foldl' (flip (:)) [])

pFoldr' :: [(Int, Int)] -> Property
pFoldr' = (List.sort . M.foldr' (:) []) `eq` (List.sort . HM.foldr' (:) [])

------------------------------------------------------------------------
-- ** Filter

pMapMaybeWithKey :: [(Key, Int)] -> Property
pMapMaybeWithKey = M.mapMaybeWithKey f `eq_` HM.mapMaybeWithKey f
  where f k v = guard (odd (keyToInt k + v)) >> Just (v + 1)

pMapMaybe :: [(Key, Int)] -> Property
pMapMaybe = M.mapMaybe f `eq_` HM.mapMaybe f
  where f v = guard (odd v) >> Just (v + 1)

pFilter :: [(Key, Int)] -> Property
pFilter = M.filter odd `eq_` HM.filter odd

pFilterWithKey :: [(Key, Int)] -> Property
pFilterWithKey = M.filterWithKey p `eq_` HM.filterWithKey p
  where p k v = odd (keyToInt k + v)

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
    , testProperty "map" pMap
    , testProperty "traverse" pTraverse
    , testProperty "mapKeys" pMapKeys
    -- Folds
    , testGroup "folds"
      [ testProperty "foldr" pFoldr
      , testProperty "foldl" pFoldl
      , testProperty "bifoldMap" pBifoldMap
      , testProperty "bifoldr" pBifoldr
      , testProperty "bifoldl" pBifoldl
      , testProperty "foldrWithKey" pFoldrWithKey
      , testProperty "foldlWithKey" pFoldlWithKey
      , testProperty "foldrWithKey'" pFoldrWithKey'
      , testProperty "foldlWithKey'" pFoldlWithKey'
      , testProperty "foldl'" pFoldl'
      , testProperty "foldr'" pFoldr'
      , testProperty "foldMapWithKey" pFoldMapWithKey
      ]
    , testGroup "difference and intersection"
      [ testProperty "difference" pDifference
      , testProperty "differenceWith" pDifferenceWith
      , testProperty "intersection" pIntersection
      , testProperty "intersection produces valid HashMap" pIntersectionValid
      , testProperty "intersectionWith" pIntersectionWith
      , testProperty "intersectionWithKey" pIntersectionWithKey
      ]
    -- Filter
    , testGroup "filter"
      [ testProperty "filter" pFilter
      , testProperty "filterWithKey" pFilterWithKey
      , testProperty "mapMaybe" pMapMaybe
      , testProperty "mapMaybeWithKey" pMapMaybeWithKey
      ]
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
