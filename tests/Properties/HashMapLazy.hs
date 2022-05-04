{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Test.QuickCheck.Function    (Fun, apply)
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
-- ** Basic interface

pSize :: [(Key, Int)] -> Property
pSize = M.size `eq` HM.size

pMember :: Key -> [(Key, Int)] -> Property
pMember k = M.member k `eq` HM.member k

pLookup :: Key -> [(Key, Int)] -> Property
pLookup k = M.lookup k `eq` HM.lookup k

pLookupOperator :: Key -> [(Key, Int)] -> Property
pLookupOperator k = M.lookup k `eq` (HM.!? k)

pInsert :: Key -> Int -> [(Key, Int)] -> Property
pInsert k v = M.insert k v `eq_` HM.insert k v

pDelete :: Key -> [(Key, Int)] -> Property
pDelete k = M.delete k `eq_` HM.delete k

newtype AlwaysCollide = AC Int
    deriving (Arbitrary, Eq, Ord, Show)

instance Hashable AlwaysCollide where
    hashWithSalt _ _ = 1

-- White-box test that tests the case of deleting one of two keys from
-- a map, where the keys' hash values collide.
pDeleteCollision :: AlwaysCollide -> AlwaysCollide -> AlwaysCollide -> Int
                 -> Property
pDeleteCollision k1 k2 k3 idx = (k1 /= k2) && (k2 /= k3) && (k1 /= k3) ==>
                                HM.member toKeep $ HM.delete toDelete $
                                HM.fromList [(k1, 1 :: Int), (k2, 2), (k3, 3)]
  where
    which = idx `mod` 3
    toDelete
        | which == 0 = k1
        | which == 1 = k2
        | which == 2 = k3
        | otherwise = error "Impossible"
    toKeep
        | which == 0 = k2
        | which == 1 = k3
        | which == 2 = k1
        | otherwise = error "Impossible"

pInsertWith :: Key -> [(Key, Int)] -> Property
pInsertWith k = M.insertWith (+) k 1 `eq_` HM.insertWith (+) k 1

pAdjust :: Key -> [(Key, Int)] -> Property
pAdjust k = M.adjust succ k `eq_` HM.adjust succ k

pUpdateAdjust :: Key -> [(Key, Int)] -> Property
pUpdateAdjust k = M.update (Just . succ) k `eq_` HM.update (Just . succ) k

pUpdateDelete :: Key -> [(Key, Int)] -> Property
pUpdateDelete k = M.update (const Nothing) k `eq_` HM.update (const Nothing) k

pAlterAdjust :: Key -> [(Key, Int)] -> Property
pAlterAdjust k = M.alter (fmap succ) k `eq_` HM.alter (fmap succ) k

pAlterInsert :: Key -> [(Key, Int)] -> Property
pAlterInsert k = M.alter (const $ Just 3) k `eq_` HM.alter (const $ Just 3) k

pAlterDelete :: Key -> [(Key, Int)] -> Property
pAlterDelete k = M.alter (const Nothing) k `eq_` HM.alter (const Nothing) k


-- We choose the list functor here because we don't fuss with
-- it in alterF rules and because it has a sufficiently interesting
-- structure to have a good chance of breaking if something is wrong.
pAlterF :: Key -> Fun (Maybe A) [Maybe A] -> [(Key, A)] -> Property
pAlterF k f xs =
  fmap M.toAscList (M.alterF (apply f) k (M.fromList xs))
  ===
  fmap toAscList (HM.alterF (apply f) k (HM.fromList xs))

pAlterFAdjust :: Key -> [(Key, Int)] -> Property
pAlterFAdjust k =
  runIdentity . M.alterF (Identity . fmap succ) k `eq_`
  runIdentity . HM.alterF (Identity . fmap succ) k

pAlterFInsert :: Key -> [(Key, Int)] -> Property
pAlterFInsert k =
  runIdentity . M.alterF (const . Identity . Just $ 3) k `eq_`
  runIdentity . HM.alterF (const . Identity . Just $ 3) k

pAlterFInsertWith :: Key -> Fun Int Int -> [(Key, Int)] -> Property
pAlterFInsertWith k f =
  runIdentity . M.alterF (Identity . Just . maybe 3 (apply f)) k `eq_`
  runIdentity . HM.alterF (Identity . Just . maybe 3 (apply f)) k

pAlterFDelete :: Key -> [(Key, Int)] -> Property
pAlterFDelete k =
  runIdentity . M.alterF (const (Identity Nothing)) k `eq_`
  runIdentity . HM.alterF (const (Identity Nothing)) k

pAlterFLookup :: Key
              -> Fun (Maybe A) B
              -> [(Key, A)] -> Property
pAlterFLookup k f =
  getConst . M.alterF (Const . apply f :: Maybe A -> Const B (Maybe A)) k
  `eq`
  getConst . HM.alterF (Const . apply f) k

pSubmap :: [(Key, Int)] -> [(Key, Int)] -> Property
pSubmap xs ys = M.isSubmapOf (M.fromList xs) (M.fromList ys) ===
                HM.isSubmapOf (HM.fromList xs) (HM.fromList ys)

pSubmapReflexive :: HashMap Key Int -> Bool
pSubmapReflexive m = HM.isSubmapOf m m

pSubmapUnion :: HashMap Key Int -> HashMap Key Int -> Bool
pSubmapUnion m1 m2 = HM.isSubmapOf m1 (HM.union m1 m2)

pNotSubmapUnion :: HashMap Key Int -> HashMap Key Int -> Property
pNotSubmapUnion m1 m2 = not (HM.isSubmapOf m1 m2) ==> HM.isSubmapOf m1 (HM.union m1 m2)

pSubmapDifference :: HashMap Key Int -> HashMap Key Int -> Bool
pSubmapDifference m1 m2 = HM.isSubmapOf (HM.difference m1 m2) m1

pNotSubmapDifference :: HashMap Key Int -> HashMap Key Int -> Property
pNotSubmapDifference m1 m2 =
  not (HM.null (HM.intersection m1 m2)) ==>
  not (HM.isSubmapOf m1 (HM.difference m1 m2))

pSubmapDelete :: HashMap Key Int -> Property
pSubmapDelete m = not (HM.null m) ==>
  forAll (elements (HM.keys m)) $ \k ->
  HM.isSubmapOf (HM.delete k m) m

pNotSubmapDelete :: HashMap Key Int -> Property
pNotSubmapDelete m =
  not (HM.null m) ==>
  forAll (elements (HM.keys m)) $ \k ->
  not (HM.isSubmapOf m (HM.delete k m))

pSubmapInsert :: Key -> Int -> HashMap Key Int -> Property
pSubmapInsert k v m = not (HM.member k m) ==> HM.isSubmapOf m (HM.insert k v m)

pNotSubmapInsert :: Key -> Int -> HashMap Key Int -> Property
pNotSubmapInsert k v m = not (HM.member k m) ==> not (HM.isSubmapOf (HM.insert k v m) m)

------------------------------------------------------------------------
-- ** Combine

pUnion :: [(Key, Int)] -> [(Key, Int)] -> Property
pUnion xs ys = M.union (M.fromList xs) `eq_` HM.union (HM.fromList xs) $ ys

pUnionWith :: [(Key, Int)] -> [(Key, Int)] -> Property
pUnionWith xs ys = M.unionWith (-) (M.fromList xs) `eq_`
                   HM.unionWith (-) (HM.fromList xs) $ ys

pUnionWithKey :: [(Key, Int)] -> [(Key, Int)] -> Property
pUnionWithKey xs ys = M.unionWithKey go (M.fromList xs) `eq_`
                             HM.unionWithKey go (HM.fromList xs) $ ys
  where
    go :: Key -> Int -> Int -> Int
    go k i1 i2 = keyToInt k - i1 + i2

pUnions :: [[(Key, Int)]] -> Property
pUnions xss = M.toAscList (M.unions (map M.fromList xss)) ===
              toAscList (HM.unions (map HM.fromList xss))

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
          \(xs :: [(Key, Int)]) -> (M.fromList xs ==) `eq` (HM.fromList xs ==)
        , testProperty "/=" $
          \(xs :: [(Key, Int)]) -> (M.fromList xs /=) `eq` (HM.fromList xs /=)
        ]
      , testGroup "Ord"
        [ testProperty "compare reflexive" $
          \(m :: HashMap Key Int) -> compare m m === EQ
        , testProperty "compare transitive" $
          \(x :: HashMap Key Int) y z -> case (compare x y, compare y z) of
            (EQ, o)  -> compare x z === o
            (o,  EQ) -> compare x z === o
            (LT, LT) -> compare x z === LT
            (GT, GT) -> compare x z === GT
            (LT, GT) -> property True -- ys greater than xs and zs.
            (GT, LT) -> property True
        , testProperty "compare antisymmetric" $
          \(x :: HashMap Key Int) y -> case (compare x y, compare y x) of
            (EQ, EQ) -> True
            (LT, GT) -> True
            (GT, LT) -> True
            _        -> False
        , testProperty "Ord => Eq" $
          \(x :: HashMap Key Int) y -> case (compare x y, x == y) of
            (EQ, True)  -> True
            (LT, False) -> True
            (GT, False) -> True
            _           -> False
        ]
      , testProperty "Read/Show" $
        \(x :: HashMap Key Int) -> x === read (show x)
      , testProperty "Functor" $
        fmap @(_ Key) @Int (+ 1) `eq_` fmap (+ 1)
      , testProperty "Foldable" $
        (List.sort @Int . Foldable.foldr @(_ Key) (:) []) `eq` (List.sort . Foldable.foldr (:) [])
      , testProperty "Hashable" $
        \(xs :: [(Key, Int)]) is salt ->
          let
            xs' = List.nubBy (\(k,_) (k',_) -> k == k') xs
            -- Shuffle the list using indexes in the second
            shuffle :: [Int] -> [a] -> [a]
            shuffle idxs = List.map snd
                         . List.sortBy (comparing fst)
                         . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
            ys = shuffle is xs'
            x = HM.fromList xs'
            y = HM.fromList ys
          in x == y ==> hashWithSalt salt x === hashWithSalt salt y
      ]
    -- Basic interface
    , testGroup "basic interface"
      [ testProperty "size" pSize
      , testProperty "member" pMember
      , testProperty "lookup" pLookup
      , testProperty "!?" pLookupOperator
      , testProperty "insert" pInsert
      , testProperty "delete" pDelete
      , testProperty "deleteCollision" pDeleteCollision
      , testProperty "insertWith" pInsertWith
      , testProperty "adjust" pAdjust
      , testProperty "updateAdjust" pUpdateAdjust
      , testProperty "updateDelete" pUpdateDelete
      , testProperty "alterAdjust" pAlterAdjust
      , testProperty "alterInsert" pAlterInsert
      , testProperty "alterDelete" pAlterDelete
      , testProperty "alterF" pAlterF
      , testProperty "alterFAdjust" pAlterFAdjust
      , testProperty "alterFInsert" pAlterFInsert
      , testProperty "alterFInsertWith" pAlterFInsertWith
      , testProperty "alterFDelete" pAlterFDelete
      , testProperty "alterFLookup" pAlterFLookup
      , testGroup "isSubmapOf"
        [ testProperty "container compatibility" pSubmap
        , testProperty "m ⊆ m" pSubmapReflexive
        , testProperty "m1 ⊆ m1 ∪ m2" pSubmapUnion
        , testProperty "m1 ⊈ m2  ⇒  m1 ∪ m2 ⊈ m1" pNotSubmapUnion
        , testProperty "m1\\m2 ⊆ m1" pSubmapDifference
        , testProperty "m1 ∩ m2 ≠ ∅  ⇒  m1 ⊈ m1\\m2 " pNotSubmapDifference
        , testProperty "delete k m ⊆ m" pSubmapDelete
        , testProperty "m ⊈ delete k m " pNotSubmapDelete
        , testProperty "k ∉ m  ⇒  m ⊆ insert k v m" pSubmapInsert
        , testProperty "k ∉ m  ⇒  insert k v m ⊈ m" pNotSubmapInsert
        ]
      ]
    -- Combine
    , testProperty "union" pUnion
    , testProperty "unionWith" pUnionWith
    , testProperty "unionWithKey" pUnionWithKey
    , testProperty "unions" pUnions
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
   -> (HM.HashMap k v -> a)  -- ^ Function that modified a 'HashMap' in the same
                             -- way
   -> [(k, v)]               -- ^ Initial content of the 'HashMap' and 'Model'
   -> Property
eq f g xs = f (M.fromList xs) === g (HM.fromList xs)

infix 4 `eq`

eq_ :: (Eq k, Eq v, Hashable k, Ord k, Show k, Show v)
    => (Model k v -> Model k v)            -- ^ Function that modifies a 'Model'
    -> (HM.HashMap k v -> HM.HashMap k v)  -- ^ Function that modified a
                                           -- 'HashMap' in the same way
    -> [(k, v)]                            -- ^ Initial content of the 'HashMap'
                                           -- and 'Model'
    -> Property
eq_ f g = (M.toAscList . f) `eq` (toAscList . g)

infix 4 `eq_`

------------------------------------------------------------------------
-- * Helpers

sortByKey :: Ord k => [(k, v)] -> [(k, v)]
sortByKey = List.sortBy (compare `on` fst)

toAscList :: Ord k => HM.HashMap k v -> [(k, v)]
toAscList = List.sortBy (compare `on` fst) . HM.toList
