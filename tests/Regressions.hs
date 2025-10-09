{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module Regressions (tests) where

import Control.Exception     (evaluate)
import Control.Monad         (replicateM)
import Data.Bits             (shiftL)
import Data.Hashable         (Hashable (..))
import Data.List             (delete)
import Data.Maybe            (isJust, isNothing)
import GHC.Exts              (touch#)
import GHC.IO                (IO (..))
import Numeric.Natural       (Natural)
import System.Mem            (performGC)
import System.Mem.Weak       (deRefWeak, mkWeakPtr)
import System.Random         (randomIO)
import Test.HUnit            (Assertion, assert)
import Test.QuickCheck
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.HUnit      (testCase)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.HashMap.Lazy   as HML
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Test.Tasty          as Tasty

import qualified Data.Foldable  as Foldable
import           NoThunks.Class (noThunksInValues)

issue32 :: Assertion
issue32 = assert $ isJust $ HMS.lookup 7 m'
  where
    ns = [0..16] :: [Int]
    m = HMS.fromList (zip ns (repeat []))
    m' = HMS.delete 10 m

------------------------------------------------------------------------
-- Issue #39

-- First regression

issue39 :: Assertion
issue39 = assert $ hm1 == hm2
  where
    hm1 = HMS.fromList ([a, b] `zip` [1, 1 :: Int ..])
    hm2 = HMS.fromList ([b, a] `zip` [1, 1 :: Int ..])
    a = (1, -1) :: (Int, Int)
    b = (-1, 1) :: (Int, Int)

-- Second regression

newtype Keys = Keys [Int]
  deriving Show

instance Arbitrary Keys where
  arbitrary = sized $ \l -> do
    pis <- replicateM (l+1) positiveInt
    return (Keys $ prefixSum pis)

  shrink (Keys ls) =
    let l = length ls
    in if l == 1
          then []
          else [ Keys (dropAt i ls) | i <- [0..l-1] ]

positiveInt :: Gen Int
positiveInt = (+1) . abs <$> arbitrary

prefixSum :: [Int] -> [Int]
prefixSum = loop 0
  where
    loop _      []     = []
    loop prefix (l:ls) = let n = l + prefix
                         in n : loop n ls

dropAt :: Int -> [a] -> [a]
dropAt _ []                 = []
dropAt i (l:ls) | i == 0    = ls
                | otherwise = l : dropAt (i-1) ls

propEqAfterDelete :: Keys -> Bool
propEqAfterDelete (Keys keys) =
  let keyMap = mapFromKeys keys
      k      = head keys
  in  HMS.delete k keyMap == mapFromKeys (delete k keys)

mapFromKeys :: [Int] -> HMS.HashMap Int ()
mapFromKeys keys = HMS.fromList (zip keys (repeat ()))

------------------------------------------------------------------------
-- Issue #254

-- Key type that always collides.
newtype KC = KC Int
  deriving (Eq, Ord, Show)
instance Hashable KC where
  hashWithSalt salt _ = salt

touch :: a -> IO ()
touch a = IO (\s -> (# touch# a s, () #))

-- We want to make sure that old values in the HashMap are evicted when new values are inserted,
-- even if they aren't evaluated. To do that, we use the WeakPtr trick described at
-- http://simonmar.github.io/posts/2018-06-20-Finding-fixing-space-leaks.html.
-- We insert a value named oldV into the HashMap, then insert over it, checking oldV is no longer reachable.
--
-- To make the test robust, it's important that oldV isn't hoisted up to the top or shared.
-- To do that, we generate it randomly.
issue254Lazy :: Assertion
issue254Lazy = do
  i :: Int <- randomIO
  let oldV = error $ "Should not be evaluated: " ++ show i
  weakV <- mkWeakPtr oldV Nothing -- add the ability to test whether oldV is alive
  mp <- evaluate $ HML.insert (KC 1) (error "Should not be evaluated") $ HML.fromList [(KC 0, "1"), (KC 1, oldV)]
  performGC
  res <- deRefWeak weakV -- gives Just if oldV is still alive
  touch mp -- makes sure that we didn't GC away the whole HashMap, just oldV
  assert $ isNothing res

-- Like issue254Lazy, but using strict HashMap
issue254Strict :: Assertion
issue254Strict = do
  i :: Int <- randomIO
  let oldV = show i
  weakV <- mkWeakPtr oldV Nothing
  mp <- evaluate $ HMS.insert (KC 1) "3" $ HMS.fromList [(KC 0, "1"), (KC 1, oldV)]
  performGC
  res <- deRefWeak weakV
  touch mp
  assert $ isNothing res

------------------------------------------------------------------------
-- Issue #379


issue379Union :: Assertion
issue379Union = do
  let m0 = HMS.fromList [(KC 1, ()), (KC 2, ())]
  let m1 = HMS.fromList [(KC 2, ()), (KC 3, ())]
  let u = m0 `HMS.union` m1
  mThunkInfo <- noThunksInValues mempty (Foldable.toList u)
  assert $ isNothing mThunkInfo

issue379StrictUnionWith :: Assertion
issue379StrictUnionWith = do
  let m0 = HMS.fromList [(KC 1, 10), (KC 2, 20 :: Int)]
  let m1 = HMS.fromList [(KC 2, 20), (KC 3, 30)]
  let u = HMS.unionWith (+) m0 m1
  mThunkInfo <- noThunksInValues mempty (Foldable.toList u)
  assert $ isNothing mThunkInfo

issue379StrictUnionWithKey :: Assertion
issue379StrictUnionWithKey = do
  let m0 = HMS.fromList [(KC 1, 10), (KC 2, 20 :: Int)]
  let m1 = HMS.fromList [(KC 2, 20), (KC 3, 30)]
  let u = HMS.unionWithKey (\(KC i) v0 v1 -> i + v0 + v1) m0 m1
  mThunkInfo <- noThunksInValues mempty (Foldable.toList u)
  assert $ isNothing mThunkInfo

-- Another key type that always collides.
--
-- Note (sjakobi): The KC newtype of Int somehow can't be used to demonstrate
-- the space leak in issue379LazyUnionWith. This type does the trick.
newtype SC = SC String
  deriving (Eq, Ord, Show)
instance Hashable SC where
  hashWithSalt salt _ = salt

issue379LazyUnionWith :: Assertion
issue379LazyUnionWith = do
  i :: Int <- randomIO
  let k = SC (show i)
  weakK <- mkWeakPtr k Nothing -- add the ability to test whether k is alive
  let f :: Int -> Int
      f x = error ("Should not be evaluated " ++ show x)
  let m = HML.fromList [(SC "1", f 1), (SC "2", f 2), (k, f 3)]
  let u = HML.unionWith (+) m m
  Just v <- evaluate $ HML.lookup k u
  performGC
  res <- deRefWeak weakK -- gives Just if k is still alive
  touch v -- makes sure that we didn't GC away the combined value
  assert $ isNothing res

------------------------------------------------------------------------
-- Issue #381

issue381mapMaybe :: Assertion
issue381mapMaybe = do
  let m0 = HMS.fromList [(KC 1, 10), (KC 2, 20 :: Int)]
  let m1 = HMS.mapMaybe (Just . (+ 1)) m0
  mThunkInfo <- noThunksInValues mempty (Foldable.toList m1)
  assert $ isNothing mThunkInfo

issue381mapMaybeWithKey :: Assertion
issue381mapMaybeWithKey = do
  let m0 = HMS.fromList [(KC 1, 10), (KC 2, 20 :: Int)]
  let m1 = HMS.mapMaybeWithKey (\(KC k) v -> Just (k + v)) m0
  mThunkInfo <- noThunksInValues mempty (Foldable.toList m1)
  assert $ isNothing mThunkInfo

------------------------------------------------------------------------
-- Issue #382

issue382 :: Assertion
issue382 = do
  i :: Int <- randomIO
  let k = SC (show i)
  weakK <- mkWeakPtr k Nothing -- add the ability to test whether k is alive
  let f :: Int -> Int -> Int
      f x = error ("Should not be evaluated " ++ show x)
  let m = HML.fromListWith f [(k, 1), (k, 2)]
  Just v <- evaluate $ HML.lookup k m
  performGC
  res <- deRefWeak weakK -- gives Just if k is still alive
  touch v -- makes sure that we didn't GC away the combined value
  assert $ isNothing res

------------------------------------------------------------------------
-- Issue #383

-- Custom Functor to prevent interference from alterF rules
newtype MyIdentity a = MyIdentity a
instance Functor MyIdentity where
  fmap f (MyIdentity x) = MyIdentity (f x)

issue383 :: Assertion
issue383 = do
  i :: Int <- randomIO
  let f Nothing = MyIdentity (Just (fromIntegral @Int @Natural (abs i)))
      f Just{}  = MyIdentity (error "Impossible")
  let (MyIdentity m) = HMS.alterF f () mempty
  mThunkInfo <- noThunksInValues mempty (Foldable.toList m)
  assert $ isNothing mThunkInfo

------------------------------------------------------------------------
-- Issue #420

issue420 :: Assertion
issue420 = do
  let k1 :: Int = 1 `shiftL` 10
  let k2 :: Int = 2 `shiftL` 10
  let s0 = HS.fromList [k1, k2]
  let s1 = s0 `HS.intersection` s0
  assert $ k1 `HS.member` s1
  assert $ k2 `HS.member` s1

------------------------------------------------------------------------
-- Issue 491

issue491 :: TestTree
issue491 = Tasty.localOption (Tasty.mkTimeout 1000000) $ testGroup "issue491" $
    [ testCase "1" $ assert $ m [0, -1] `HML.isSubmapOf` m [0, -1]
    , testCase "2" $ assert $ m [1, 0b11111] `HML.isSubmapOf` m [1, 0b11111]
    , testCase "3" $ assert $ m [0, 1] `HML.isSubmapOf` m [0, 1, 0b11111]
    ]
  where m = HS.toMap . HS.fromList @Int

------------------------------------------------------------------------
-- * Test list

tests :: TestTree
tests = testGroup "Regression tests"
    [
      testCase "issue32" issue32
    , testCase "issue39a" issue39
    , testProperty "issue39b" propEqAfterDelete
    , testCase "issue254 lazy" issue254Lazy
    , testCase "issue254 strict" issue254Strict
    , testGroup "issue379"
          [ testCase "Lazy.unionWith" issue379LazyUnionWith
          , testCase "union" issue379Union
          , testCase "Strict.unionWith" issue379StrictUnionWith
          , testCase "Strict.unionWithKey" issue379StrictUnionWithKey
          ]
    , testGroup "issue381"
          [ testCase "mapMaybe" issue381mapMaybe
          , testCase "mapMaybeWithKey" issue381mapMaybeWithKey
          ]
    , testCase "issue382" issue382
    , testCase "issue383" issue383
    , testCase "issue420" issue420
    , issue491
    ]
