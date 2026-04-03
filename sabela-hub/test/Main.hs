module Main where

import qualified Test.AuthSpec
import Test.Hspec
import qualified Test.ProxySpec
import qualified Test.SessionSpec

main :: IO ()
main = hspec $ do
    Test.AuthSpec.spec
    Test.SessionSpec.spec
    Test.ProxySpec.spec
