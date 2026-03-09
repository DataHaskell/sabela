module Main (main) where

import Test.Hspec (hspec)
import qualified Test.SessionSpec as SessionSpec
import qualified Test.TopoSpec as TopoSpec

main :: IO ()
main = hspec $ do
    SessionSpec.spec
    TopoSpec.spec
