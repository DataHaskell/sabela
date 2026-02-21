module Main (main) where

import Test.Hspec (hspec)
import qualified Test.SessionSpec as SessionSpec

main :: IO ()
main = hspec SessionSpec.spec
