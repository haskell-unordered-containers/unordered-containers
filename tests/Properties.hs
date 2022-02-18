module Properties (tests) where

import Test.Tasty (TestTree, testGroup)

import qualified Properties.HashMapLazy
import qualified Properties.HashMapStrict
import qualified Properties.HashSet
import qualified Properties.List

tests :: TestTree
tests = testGroup "Properties"
  [ Properties.HashMapLazy.tests
  , Properties.HashMapStrict.tests
  , Properties.HashSet.tests
  , Properties.List.tests
  ]
