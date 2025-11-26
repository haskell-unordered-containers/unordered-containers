{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans            #-} -- because of the Arbitrary instances
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- https://github.com/nick8325/quickcheck/issues/344

-- | Tests for the 'Data.HashSet' module.  We test functions by
-- comparing them to @Set@ from @containers@. @Set@ is referred to as a
-- /model/ for @HashSet@.

module Properties.HashSet (tests) where

import Data.Hashable         (Hashable (hashWithSalt))
import Data.HashMap.Lazy     (HashMap)
import Data.HashSet          (HashSet)
import Data.Ord              (comparing)
import Data.Set              (Set)
import Test.QuickCheck       (Fun, pattern Fn, (===), (==>))
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), testProperty)
import Util.Key              (Key, keyToInt)

import qualified Data.Foldable     as Foldable
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.List         as List
import qualified Data.Set          as S
import qualified Test.QuickCheck   as QC

instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = HM.fromList <$> arbitrary
  shrink = fmap HM.fromList . shrink . HM.toList

instance (Hashable a, Arbitrary a) => Arbitrary (HashSet a) where
  arbitrary = HS.fromMap <$> arbitrary
  shrink = fmap HS.fromMap . shrink . HS.toMap

------------------------------------------------------------------------
-- Helpers

type HSK = HashSet Key

toOrdSet :: Ord a => HashSet a -> Set a
toOrdSet = S.fromList . HS.toList

------------------------------------------------------------------------
-- Test list

tests :: TestTree
tests = testGroup "Data.HashSet"
  [ -- Instances
    testGroup "instances"
    [ testGroup "Eq"
      [ testProperty "==" $
        \(x :: HSK) y -> (x == y) === (toOrdSet x == toOrdSet y)
      , testProperty "== permutations" $
        \(xs :: [Key]) (is :: [Int]) ->
          let shuffle idxs = List.map snd
                           . List.sortBy (comparing fst)
                           . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
              ys = shuffle is xs
          in  HS.fromList xs === HS.fromList ys
      , testProperty "/=" $
        \(x :: HSK) y -> (x /= y) === (toOrdSet x /= toOrdSet y)
      ]
    , testGroup "Ord"
      [ testProperty "compare reflexive" $
        -- We cannot compare to `Data.Map` as ordering is different.
        \(x :: HSK) -> compare x x === EQ
      , testProperty "compare transitive" $
        \(x :: HSK) y z -> case (compare x y, compare y z) of
          (EQ, o)  -> compare x z === o
          (o,  EQ) -> compare x z === o
          (LT, LT) -> compare x z === LT
          (GT, GT) -> compare x z === GT
          (LT, GT) -> QC.property True -- ys greater than xs and zs.
          (GT, LT) -> QC.property True
      , testProperty "compare antisymmetric" $
        \(x :: HSK) y -> case (compare x y, compare y x) of
          (EQ, EQ) -> True
          (LT, GT) -> True
          (GT, LT) -> True
          _        -> False
      , testProperty "Ord => Eq" $
        \(x :: HSK) y -> case (compare x y, x == y) of
          (EQ, True)  -> True
          (LT, False) -> True
          (GT, False) -> True
          _           -> False
      ]
    , testProperty "Read/Show" $
      \(x :: HSK) -> x === read (show x)
    , testProperty "Foldable" $
      \(x :: HSK) ->
        List.sort (Foldable.foldr (:) [] x)
        ===
        List.sort (Foldable.foldr (:) [] (toOrdSet x))
    , testProperty "Hashable" $
      \(xs :: [Key]) (is :: [Int]) salt ->
        let shuffle idxs = List.map snd
                 . List.sortBy (comparing fst)
                 . List.zip (idxs ++ [List.maximum (0:is) + 1 ..])
            xs' = List.nub xs
            ys = shuffle is xs'
            x = HS.fromList xs'
            y = HS.fromList ys
        in  x == y ==> hashWithSalt salt x === hashWithSalt salt y
    ]
  -- Basic interface
  , testProperty "size" $
    \(x :: HSK) -> HS.size x === List.length (HS.toList x)
  , testProperty "member" $
    \e (s :: HSK) -> HS.member e s === S.member e (toOrdSet s)
  , testProperty "insert" $
    \e (s :: HSK) -> toOrdSet (HS.insert e s) === S.insert e (toOrdSet s)
  , testProperty "delete" $
    \e (s :: HSK) -> toOrdSet (HS.delete e s) === S.delete e (toOrdSet s)
  -- Combine
  , testProperty "union" $
    \(x :: HSK) y -> toOrdSet (HS.union x y) === S.union (toOrdSet x) (toOrdSet y)
  -- Transformations
  , testProperty "map" $
    \(Fn f :: Fun Key Key) (s :: HSK) -> toOrdSet (HS.map f s) === S.map f (toOrdSet s)
  -- Folds
  , testProperty "foldr" $
    \(s :: HSK) ->
      List.sort (HS.foldr (:) [] s) === List.sort (S.foldr (:) [] (toOrdSet s))
  , testProperty "foldl'" $
    \(s :: HSK) z0 ->
      let f z k = keyToInt k + z
      in  HS.foldl' f z0 s === S.foldl' f z0 (toOrdSet s)
  -- Filter
  , testProperty "filter" $
    \(Fn p) (s :: HSK) -> toOrdSet (HS.filter p s) === S.filter p (toOrdSet s)
  -- Conversions
  , testProperty "toList" $
    \(xs :: [Key]) -> List.sort (HS.toList (HS.fromList xs)) === S.toAscList (S.fromList xs)
  ]
