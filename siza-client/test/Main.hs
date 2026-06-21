module Main (main) where

import Test.CrossSeamSpec (crossSeamSpec)
import Test.Hspec
import Test.LanguageSpec (
    annotateSpec,
    contractSpec,
    parseSpec,
    securitySpec,
 )
import Test.ProvenanceSpec (chainSpec, provenanceSpec, retroSpec)

main :: IO ()
main = hspec $ do
    parseSpec
    securitySpec
    annotateSpec
    contractSpec
    provenanceSpec
    chainSpec
    retroSpec
    crossSeamSpec
