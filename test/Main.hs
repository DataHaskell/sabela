module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.AllSpecsA (allSpecsA)
import Test.AllSpecsB (allSpecsB)
import Test.Hspec (hspec)

main :: IO ()
main = do
    setLocaleEncoding utf8
    hspec $ do
        allSpecsA
        allSpecsB
