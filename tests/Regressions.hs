{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
module Regressions (tests) where

import Control.Exception     (evaluate)
import Control.Monad         (replicateM)
import Data.Hashable         (Hashable (..))
import Data.List             (delete)
import Data.Maybe            (isJust, isNothing)
import GHC.Exts              (touch#)
import GHC.IO                (IO (..))
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
-- * Test list

tests :: TestTree
tests = testGroup "Regression tests"
    [
      testCase "issue32" issue32
    , testCase "issue39a" issue39
    , testProperty "issue39b" propEqAfterDelete
    , testCase "issue254 lazy" issue254Lazy
    , testCase "issue254 strict" issue254Strict
    ]
