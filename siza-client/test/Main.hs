module Main (main) where

import Test.CrossSeamSpec (crossSeamSpec)
import Test.Hspec
import Test.HubTokenSpec (hubTokenSpec)
import Test.LanguageSpec (
    annotateSpec,
    contractSpec,
    parseSpec,
    securitySpec,
 )
import Test.LoginSpec (loginSpec)
import Test.McpSpec (mcpSpec)
import Test.ProvenanceSpec (chainSpec, provenanceSpec, retroSpec)
import Test.TransportSpec (transportSpec)

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
    transportSpec
    hubTokenSpec
    loginSpec
    mcpSpec
