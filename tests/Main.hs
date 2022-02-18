module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Regressions
import qualified Properties
import qualified Strictness

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Properties.tests
  , Regressions.tests
  , Strictness.tests
  ]
