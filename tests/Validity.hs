{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import GHC.Generics (Generic)

import Data.Bits (popCount)
import Data.Foldable (foldl')
import Data.List (nub)
import Data.Validity
import Control.Monad (foldM)
import Data.Hashable (Hashable(hashWithSalt))
#if defined(STRICT)
import qualified Data.HashMap.Strict as HM
#else
import qualified Data.HashMap.Lazy as HM
#endif
import Data.HashMap.Base (HashMap(..), Leaf(..), Salt, Hash)
import qualified Data.HashMap.Base as HM (defaultSalt, hashWithSalt, nextSalt, bitsPerSubkey)
import qualified Data.HashMap.Array as A

import Data.Validity (Validity)
import Data.GenValidity (GenUnchecked(..), GenValid(..), genSplit, genSplit4)
import Test.Validity (producesValidsOnValids, producesValidsOnValids2, producesValidsOnValids3, shouldBeValid)
import Test.QuickCheck (Arbitrary, CoArbitrary, Function, Property, Gen, sized, oneof, resize, scale, suchThat, forAll)
import Test.Framework(Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Function (Fun, apply, applyFun2, applyFun3)

instance (Validity k, Validity v) => Validity (Leaf k v) where
    validate (L k v) = mconcat
        [ annotate k "key"
        , annotate v "value"
        ]

instance Validity a => Validity (A.Array a) where
    validate a = annotate (A.toList a) "The array elements"

instance (Eq k, Hashable k, Validity k, Validity v) => Validity (HashMap k v) where
    validate = go HM.defaultSalt
      where
        go s hm = case hm of
            Empty -> mempty
            (Leaf h l@(L k _)) -> decorate "Leaf" $ mconcat
                [ annotate h "Hash"
                , annotate l "Leaf"
                , check (HM.hashWithSalt s k == h) "The hash is correct."
                ]
            (BitmapIndexed bm a) -> decorate "BitmapIndexed" $ mconcat
                [ annotate bm "Bitmap"
                , decorate "Array" $ decorateList (A.toList a) $ go s
                , check (A.length a == popCount bm)
                  "There are values in the array equal to popCount bm."
                , check (uniques $ concatMap HM.keys $ A.toList a) "The keys are unique."
                ]
            (Full a) -> decorate "Full" $ mconcat
                [ decorate "Array" $ decorateList (A.toList a) $ go s
                , check (A.length a == 2 ^ HM.bitsPerSubkey)
                  "There are 2 ^ bitsPerSubkey values in the array."
                , check (uniques $ concatMap HM.keys $ A.toList a) "The keys are unique."
                ]
            (Collision h l1@(L k1 _) l2@(L k2 _) hm') -> decorate "Collision" $ mconcat
                [ annotate h "Hash"
                , annotate l1 "The first collision"
                , annotate l2 "The second collision"
                , check (HM.hashWithSalt s k1 == h) "The hash of the first collision is correct."
                , check (HM.hashWithSalt s k2 == h) "The hash of the second collision is correct."
                , check (k1 /= k2) "The keys within the collision are not equal."
                , check (uniques (k1 : k2 : HM.keys hm')) "The keys are unique."
                , decorate "The recursive HashMap" $ go (HM.nextSalt s) hm'
                , check (all (\k -> HM.hashWithSalt s k == h) (HM.keys hm'))
                  "All recursive keys hash to the hash within the collision."
                ]

uniques :: Eq a => [a] -> Bool
uniques l = length (nub l) == length l

#if !MIN_VERSION_validity(0,6,0)
-- | Decorate a validation with a location
decorate :: String -> Validation -> Validation
decorate = flip annotateValidation

annotateValidation :: Validation -> String -> Validation
annotateValidation val s =
    case val of
      Validation errs -> Validation $ map (Location s) errs
#endif

#if !MIN_VERSION_validity(0,8,0)
-- | Decorate a piecewise validation of a list with their location in the list
decorateList :: [a] -> (a -> Validation) -> Validation
decorateList as func = mconcat $
    flip map (zip [0..] as) $ \(i, a) ->
        decorate (unwords ["The element at index", show (i :: Integer), "in the list"]) $
        func a
#endif

instance (GenUnchecked k, GenUnchecked v) => GenUnchecked (Leaf k v) where
    genUnchecked = sized $ \n -> do
      (a, b) <- genSplit n
      k <- resize a genUnchecked
      v <- resize b genUnchecked
      pure $ L k v
    shrinkUnchecked (L k v) = [L k' v' | (k', v') <- shrinkUnchecked (k, v)]

instance (GenValid k, GenValid v) => GenValid (Leaf k v) where
    genValid = sized $ \n -> do
      (a, b) <- genSplit n
      k <- resize a genValid
      v <- resize b genValid
      pure $ L k v

instance GenUnchecked a => GenUnchecked (A.Array a) where
    genUnchecked = do
      l <- genUnchecked
      pure $ A.fromList (length l) l
    shrinkUnchecked _ = [] -- TODO: whrite shrinking

instance GenValid a => GenValid (A.Array a) where
    genValid = do
      l <- genValid
      pure $ A.fromList (length l) l

instance (GenUnchecked k, GenUnchecked v) => GenUnchecked (HashMap k v) where
    genUnchecked = sized $ \n -> case n of
      0 -> pure Empty
      _ -> oneof
        [ do
            (a, b) <- genSplit n
            BitmapIndexed
              <$> resize a genUnchecked
              <*> resize b genUnchecked
        , do
            (a, b) <- genSplit n
            Leaf
              <$> resize a genUnchecked
              <*> resize b genUnchecked
        , Full <$> genUnchecked
        , do
            (a,b,c,d) <- genSplit4 n
            Collision
              <$> resize a genUnchecked
              <*> resize b genUnchecked
              <*> resize c genUnchecked
              <*> resize d genUnchecked
        ]
    shrinkUnchecked _ = [] -- TODO: write shrinking

-- Note: This instance has to generate collisions.
-- this can take long for keys that don't have a lot of collisions, so best
-- to only use it with the 'Key' in this module.
instance (Eq k, Hashable k, GenValid k, GenValid v) => GenValid (HashMap k v) where
    genValid = snd <$> genAndRecordKeys [] [] HM.defaultSalt
      where
        genKey :: [(Salt, Hash)] -> [k] -> Gen k
        genKey hs keys =
          foldl'
            (\g (s,h) -> modGenWith s h g) 
            (genValid `suchThat` (`notElem` keys))
            hs
          where
            modGenWith :: Salt -> Hash -> Gen k -> Gen k
            modGenWith s h g = g `suchThat` (\k -> HM.hashWithSalt s k == h)
        genL :: [(Salt, Hash)] -> [k] -> Gen (Leaf k v)
        genL hs keys = sized $ \n -> do
          (a, b) <- genSplit n
          k <- resize a $ genKey hs keys
          v <- resize b genValid
          pure $ L k v
        genFixedNbOfHms :: Int -> [(Salt, Hash)] -> [k] -> Salt -> Gen ([k], [HashMap k v])
        genFixedNbOfHms nb hs keys s = do
            let go' :: ([k], [HashMap k v]) -> Int -> Gen ([k], [HashMap k v])
                go' (ks, hms) _ = do
                  (ks', hm') <- scale (`div` nb) $ genAndRecordKeys hs ks s
                  pure (ks' ++ ks, hm': hms)
            (keys', values) <- foldM go' (keys, []) [1 .. nb]
            pure (keys', values)
        genAndRecordKeys :: [(Salt, Hash)] -> [k] -> Salt -> Gen ([k], HashMap k v)
        genAndRecordKeys hs keys s = sized $ \n -> case n of
          0 -> pure (keys, Empty)
          _ -> oneof
            [ do
                l@(L k _) <- genL hs keys
                let hm' = Leaf (HM.hashWithSalt s k) l :: HashMap k v
                pure $ (k : keys, hm')
            , do
                bm <- genValid :: Gen Word
                let pc = popCount bm :: Int
                (keys', hms) <- genFixedNbOfHms pc hs keys s :: Gen ([k], [HashMap k v])
                let hm' = BitmapIndexed bm $ A.fromList pc hms :: HashMap k v
                pure (keys', hm')
            , do
                let l = 2 ^ (4 :: Int) -- 4 == bitsPerSubkey
                (keys', hms) <- genFixedNbOfHms l hs keys s
                let hm' = Full $ A.fromList l hms :: HashMap k v
                pure (keys', hm') :: Gen ([k], HashMap k v)
            , do
                (a,b) <- genSplit n
                (l1@(L k1 _), l2@(L k2 _)) <- resize a $
                  ((,) <$> genL hs keys <*> genL hs keys)
                  `suchThat`
                  (\((L k1 _), (L k2 _)) -> HM.hashWithSalt s k1 == HM.hashWithSalt s k2 && k1 /= k2)
                let h = HM.hashWithSalt s k1
                (keys', hm') <- resize b $ genAndRecordKeys ((s, h) : hs) (k1 : k2 : keys) (HM.nextSalt s)
                pure (keys', Collision h l1 l2 hm') :: Gen ([k], HashMap k v)
            ]

-- Key type that generates more hash collisions.
newtype Key = K
    { unK :: Int }
    deriving (Arbitrary, CoArbitrary, Validity, GenUnchecked, GenValid, Eq, Ord, Read, Show, Generic)

instance Hashable Key where
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 10

instance Function Key

pSingleton :: Property
pSingleton = producesValidsOnValids2 (HM.singleton :: Key -> Int -> HM.HashMap Key Int)

pNull :: Property
pNull = producesValidsOnValids (HM.null :: HM.HashMap Key Int -> Bool)

pSize :: Property
pSize = producesValidsOnValids (HM.size :: HM.HashMap Key Int -> Int)

pMember :: Property
pMember = producesValidsOnValids2 (HM.member :: Key -> HM.HashMap Key Int -> Bool)

pLookup :: Property
pLookup = producesValidsOnValids2 (HM.lookup :: Key -> HM.HashMap Key Int -> Maybe Int)

pLookupDefault :: Property
pLookupDefault = producesValidsOnValids3 (HM.lookupDefault :: Int -> Key -> HM.HashMap Key Int -> Int)

pInsert :: Property
pInsert = producesValidsOnValids3 (HM.insert :: Key -> Int -> HM.HashMap Key Int -> HM.HashMap Key Int)

pInsertWith :: Property
pInsertWith = producesValidsOnValids3 (HM.insertWith (+) :: Key -> Int -> HM.HashMap Key Int -> HM.HashMap Key Int)

pDelete :: Property
pDelete = producesValidsOnValids2 (HM.delete :: Key -> HM.HashMap Key Int -> HM.HashMap Key Int)

pAdjust :: Fun Int Int -> Property
pAdjust f = producesValidsOnValids2 (HM.adjust (apply f) :: Key -> HM.HashMap Key Int -> HM.HashMap Key Int)

pUpdate :: (Fun Int (Maybe Int)) -> Property
pUpdate f = producesValidsOnValids2 (HM.update (apply f) :: Key -> HM.HashMap Key Int -> HM.HashMap Key Int)

pAlter :: Property
pAlter = producesValidsOnValids2 (HM.alter (fmap succ) :: Key -> HM.HashMap Key Int -> HM.HashMap Key Int)

pAlterF :: Fun (Maybe Int) [Maybe Int] -> Property
pAlterF f = producesValidsOnValids2 (HM.alterF (apply f) :: Key -> HM.HashMap Key Int -> [HM.HashMap Key Int])

pUnion :: Property
pUnion = producesValidsOnValids2 (HM.union :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pUnionWith :: Fun (Int, Int) Int -> Property
pUnionWith f = producesValidsOnValids2 (HM.unionWith (applyFun2 f) :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pUnionWithKey :: Fun (Key, Int, Int) Int -> Property
pUnionWithKey f = producesValidsOnValids2 (HM.unionWithKey (applyFun3 f) :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pUnions :: Property
pUnions = producesValidsOnValids (HM.unions :: [HashMap Key Int] -> HashMap Key Int)

pMap :: Fun Int Int -> Property
pMap f = producesValidsOnValids (HM.map (apply f) :: HashMap Key Int -> HashMap Key Int)

pMapWithKey :: Fun (Key, Int) Int -> Property
pMapWithKey f = producesValidsOnValids (HM.mapWithKey (applyFun2 f) :: HashMap Key Int -> HashMap Key Int)

pTraverseWithKey :: Fun (Key, Int) (Maybe Int) -> Property
pTraverseWithKey f = producesValidsOnValids (HM.traverseWithKey (applyFun2 f) :: HashMap Key Int -> Maybe (HashMap Key Int))

pDifference :: Property
pDifference = producesValidsOnValids2 (HM.difference :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pDifferenceWith :: Fun (Int, Int) (Maybe Int) -> Property
pDifferenceWith f = producesValidsOnValids2 (HM.differenceWith (applyFun2 f) :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pIntersection :: Property
pIntersection = producesValidsOnValids2 (HM.intersection :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pIntersectionWith :: Fun (Int, Int) Int -> Property
pIntersectionWith f = producesValidsOnValids2 (HM.intersectionWith (applyFun2 f) :: HashMap Key Int -> HashMap \
  Key Int -> HashMap Key Int)

pIntersectionWithKey :: Fun (Key, Int, Int) Int -> Property
pIntersectionWithKey f = producesValidsOnValids2 (HM.intersectionWithKey (applyFun3 f) :: HashMap Key Int -> HashMap Key Int -> HashMap Key Int)

pFoldl' :: Fun (Word, Int) Word -> Property
pFoldl' f = producesValidsOnValids2 (HM.foldl' (applyFun2 f) :: Word -> HashMap Key Int -> Word)

pFoldlWithKey' :: Fun (Word, Key, Int) Word -> Property
pFoldlWithKey' f = producesValidsOnValids2 (HM.foldlWithKey' (applyFun3 f) :: Word -> HashMap Key Int -> Word)

pFoldr :: Fun (Int, Word) Word -> Property
pFoldr f = producesValidsOnValids2 (HM.foldr (applyFun2 f) :: Word -> HashMap Key Int -> Word)

pFoldrWithKey :: Fun (Key, Int, Word) Word -> Property
pFoldrWithKey f = producesValidsOnValids2 (HM.foldrWithKey (applyFun3 f) :: Word -> HashMap Key Int -> Word)

pFilter :: Fun Int Bool -> Property
pFilter f = producesValidsOnValids (HM.filter (apply f) :: HashMap Key Int -> HashMap Key Int)

pFilterWithKey :: Fun (Key, Int) Bool -> Property
pFilterWithKey f = producesValidsOnValids (HM.filterWithKey (applyFun2 f) :: HashMap Key Int -> HashMap Key Int)

pMapMaybe :: Fun Int (Maybe Int) -> Property
pMapMaybe f = producesValidsOnValids (HM.mapMaybe (apply f) :: HashMap Key Int -> HashMap Key Int)

pMapMaybeWithKey :: Fun (Key, Int) (Maybe Int) -> Property
pMapMaybeWithKey f = producesValidsOnValids (HM.mapMaybeWithKey (applyFun2 f) :: HashMap Key Int -> HashMap Key Int)

pKeys :: Property
pKeys = producesValidsOnValids (HM.keys :: HashMap Key Int -> [Key])

pElems :: Property
pElems = producesValidsOnValids (HM.elems :: HashMap Key Int -> [Int])

pToList :: Property
pToList = producesValidsOnValids (HM.toList :: HashMap Key Int -> [(Key, Int)])

pFromList :: Property
pFromList = producesValidsOnValids (HM.fromList :: [(Key, Int)] -> HashMap Key Int)

pFromListWith :: Fun (Int, Int) Int -> Property
pFromListWith f = producesValidsOnValids (HM.fromListWith (applyFun2 f) :: [(Key, Int)] -> HashMap Key Int)


------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
  let genValidHelper gen = forAll gen shouldBeValid
  in
    [
    -- Basic interface
      testGroup "HashMap"
      [ testProperty "genValid generates valid values for Leaf" $
        genValidHelper (genValid :: Gen (Leaf Key Int))
      , testProperty "genValid generates valid values for Array" $
        genValidHelper (genValid :: Gen (A.Array Key))
      , testProperty "genValid generates valid values for HashMap" $
        genValidHelper (genValid :: Gen (HashMap Key Int))
      ]
    , testGroup "HashMap"
      [ testProperty "singleton produces valid HashMaps" pSingleton
      , testProperty "null produces valid Bools" pNull
      , testProperty "size produces valid HashMaps" pSize
      , testProperty "member produces valid HashMaps" pMember
      , testProperty "lookup produces valid HashMaps" pLookup
      , testProperty "lookupDefault produces valid HashMaps" pLookupDefault
      , testProperty "insert produces valid HashMaps" pInsert
      , testProperty "insertWith produces valid HashMaps" pInsertWith
      , testProperty "delete produces valid HashMaps" pDelete
      , testProperty "adjust produces valid HashMaps" pAdjust
      , testProperty "update produces valid HashMaps" pUpdate
      , testProperty "alter produces valid HashMaps" pAlter
      , testProperty "alterF produces valid HashMaps" pAlterF
      , testProperty "union produces valid HashMaps" pUnion
      , testProperty "unionWith produces valid HashMaps" pUnionWith
      , testProperty "unionWithKey produces valid HashMaps" pUnionWithKey
      , testProperty "unions produces valid HashMaps" pUnions
      , testProperty "map produces valid HashMaps" pMap
      , testProperty "mapWithKey produces valid HashMaps" pMapWithKey
      , testProperty "traverseWithKey produces valid HashMaps" pTraverseWithKey
      , testProperty "difference produces valid HashMaps" pDifference
      , testProperty "differenceWith produces valid HashMaps" pDifferenceWith
      , testProperty "intersection produces valid HashMaps" pIntersection
      , testProperty "intersectionWith produces valid HashMaps" pIntersectionWith
      , testProperty "intersectionWithKey produces valid HashMaps" pIntersectionWithKey
      , testProperty "foldl' produces valid HashMaps" pFoldl'
      , testProperty "foldlWithKey' produces valid HashMaps" pFoldlWithKey'
      , testProperty "foldr produces valid HashMaps" pFoldr
      , testProperty "foldrWithKey produces valid HashMaps" pFoldrWithKey
      , testProperty "filter produces valid HashMaps" pFilter
      , testProperty "filterWithKey produces valid HashMaps" pFilterWithKey
      , testProperty "mapMaybe produces valid HashMaps" pMapMaybe
      , testProperty "mapMaybeWithKey produces valid HashMaps" pMapMaybeWithKey
      , testProperty "keys produces valid lists" pKeys
      , testProperty "elems produces valid lists" pElems
      , testProperty "toList produces valid lists" pToList
      , testProperty "fromList produces valid HashMaps" pFromList
      , testProperty "fromListWith produces valid HashMaps" pFromListWith
      ]
    ]
-- TODO keysSet

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests
