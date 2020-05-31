module Main where

import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HML
import Data.List (delete)
import Data.Maybe
import System.Mem (performGC)
import System.Mem.Weak (mkWeakPtr, deRefWeak)
import Test.HUnit (Assertion, assert)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

issue32 :: Assertion
issue32 = assert $ isJust $ HM.lookup 7 m'
  where
    ns = [0..16] :: [Int]
    m = HM.fromList (zip ns (repeat []))
    m' = HM.delete 10 m

------------------------------------------------------------------------
-- Issue #39

-- First regression

issue39 :: Assertion
issue39 = assert $ hm1 == hm2
  where
    hm1 = HM.fromList ([a, b] `zip` [1, 1 :: Int ..])
    hm2 = HM.fromList ([b, a] `zip` [1, 1 :: Int ..])
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
  in  HM.delete k keyMap == mapFromKeys (delete k keys)

mapFromKeys :: [Int] -> HM.HashMap Int ()
mapFromKeys keys = HM.fromList (zip keys (repeat ()))

------------------------------------------------------------------------
-- Issue #254

data KC = KC Int
  deriving (Eq, Ord, Show)
instance Hashable KC where
  hashWithSalt salt _ = salt

issue254Lazy :: Assertion
issue254Lazy = issue254LazyLambda 2

-- Important that oldV is not hoisted out by optimisation, so use NOINLINE
{-# NOINLINE issue254LazyLambda #-}
issue254LazyLambda :: Int -> Assertion
issue254LazyLambda i = do
  _ <- return ()
  let oldV = show i
  weakV <- mkWeakPtr oldV Nothing
  let mp = HML.insert (KC 1) "3" $ HML.fromList [(KC 0, "1"), (KC 1, oldV)]
  _ <- evaluate mp
  performGC
  res <- deRefWeak weakV
  _ <- evaluate mp
  assert $ isNothing res

issue254Strict :: Assertion
issue254Strict = issue254StrictLambda 2

-- Important that oldV is not hoisted out by optimisation, so use NOINLINE
{-# NOINLINE issue254StrictLambda #-}
issue254StrictLambda :: Int -> Assertion
issue254StrictLambda i = do
  _ <- return ()
  let oldV = show i
  weakV <- mkWeakPtr oldV Nothing
  let mp = HM.insert (KC 1) "3" $ HM.fromList [(KC 0, "1"), (KC 1, oldV)]
  _ <- evaluate mp
  performGC
  res <- deRefWeak weakV
  _ <- evaluate mp
  assert $ isNothing res

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
      testCase "issue32" issue32
    , testCase "issue39a" issue39
    , testProperty "issue39b" propEqAfterDelete
    , testCase "issue254 lazy" issue254Lazy
    , testCase "issue254 strict" issue254Strict
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests
