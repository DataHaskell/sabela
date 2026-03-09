module Main (main) where

import Test.Hspec (hspec)
import qualified Test.WebDriverSpec as WebDriverSpec

main :: IO ()
main = hspec WebDriverSpec.spec
