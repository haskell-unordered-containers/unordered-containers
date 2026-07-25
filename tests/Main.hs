module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (lookupEnv, setEnv)
import Test.Tasty      (defaultMain, testGroup)

import qualified Properties
import qualified Regressions
import qualified Strictness

main :: IO ()
main = do
  setLocaleEncoding utf8
  hideSuccesses <- lookupEnv "TASTY_HIDE_SUCCESSES"
  case hideSuccesses of
    Nothing -> setEnv "TASTY_HIDE_SUCCESSES" "true"
    Just _ -> pure ()
  defaultMain $ testGroup "All"
    [ Properties.tests
    , Regressions.tests
    , Strictness.tests
    ]
