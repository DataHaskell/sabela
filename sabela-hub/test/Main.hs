module Main where

import qualified Test.AuthSpec
import qualified Test.DockerSpec
import Test.Hspec
import qualified Test.ProxySpec
import qualified Test.ReaperSpec
import qualified Test.SessionSpec
import qualified Test.ShareSpec

main :: IO ()
main = hspec $ do
    Test.AuthSpec.spec
    Test.DockerSpec.spec
    Test.ShareSpec.spec
    Test.SessionSpec.spec
    Test.ReaperSpec.spec
    Test.ProxySpec.spec
