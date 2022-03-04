module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Properties
import qualified Regressions
import qualified Strictness

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Properties.tests
  , Regressions.tests
  , Strictness.tests
  ]
