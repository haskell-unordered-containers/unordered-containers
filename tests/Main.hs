module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty      (defaultMain, testGroup)

import qualified Properties
import qualified Regressions
import qualified Strictness

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "All"
    [ Properties.tests
    , Regressions.tests
    , Strictness.tests
    ]
