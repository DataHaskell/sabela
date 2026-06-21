module Main where

import qualified Test.AdminApiSpec
import qualified Test.AllowlistSpec
import qualified Test.AuthSpec
import qualified Test.BannerSpec
import qualified Test.CliAuthSpec
import qualified Test.DockerSpec
import qualified Test.GalleryRenderSpec
import qualified Test.GallerySpec
import Test.Hspec
import qualified Test.ProxyRoutesSpec
import qualified Test.ProxySpec
import qualified Test.ReaperSpec
import qualified Test.RunnerSpec
import qualified Test.SessionSpec
import qualified Test.ShareSpec
import qualified Test.UsersSpec

main :: IO ()
main = hspec $ do
    Test.AllowlistSpec.spec
    Test.AuthSpec.spec
    Test.GallerySpec.spec
    Test.GalleryRenderSpec.spec
    Test.BannerSpec.spec
    Test.RunnerSpec.spec
    Test.CliAuthSpec.spec
    Test.AdminApiSpec.spec
    Test.DockerSpec.spec
    Test.ShareSpec.spec
    Test.UsersSpec.spec
    Test.SessionSpec.spec
    Test.ReaperSpec.spec
    Test.ProxySpec.spec
    Test.ProxyRoutesSpec.spec
