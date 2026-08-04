{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverAbsentFactsSpec (discoverAbsentFactsSpec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getTemporaryDirectory)
import System.Environment (setEnv)
import System.FilePath ((</>))
import Test.Hspec

import Test.DiscoverFixtures (
    hitText,
    hitsOf,
    installNamesFileWith,
    runCat,
    runCatArgs,
    stateOf,
    synHackageNames,
    textField,
 )

{- | A package the catalogue knows by name alone: nothing it exposes is
installed, and no session or Hoogle answer names it. This is the shape the
hodatime episode ran onto (docs/discover/live/live_hodatime.md).
-}
installFactsFile :: IO ()
installFactsFile = do
    dir <- getTemporaryDirectory
    let path = dir </> "siza-discover-test-facts.tsv"
    TIO.writeFile
        path
        ( T.unlines
            [ "vapour\thttps://example.invalid/vapour\tCondensation tooling\t\
              \Data.Vapour.Internal Data.Vapour Data.Vapour.Cloud \
              \Data.Vapour.Cloud.Layer Data.Vapour.Droplet \
              \Data.Vapour.Droplet.Size Data.Vapour.Mist"
            , "nimbus\t\tRain\tNimbus.Sky"
            ]
        )
    setEnv "SABELA_HACKAGE_FACTS" path

installBoth :: IO ()
installBoth = do
    installNamesFileWith (synHackageNames ++ ["vapour"])
    installFactsFile

discoverAbsentFactsSpec :: Spec
discoverAbsentFactsSpec =
    beforeAll_ installBoth $
        describe "what an absent-known package can state (live_hodatime)" $ do
            describe "a package known by name alone" $ do
                it "is found, not denied" $ do
                    v <- runCat "vapour"
                    stateOf v `shouldBe` "found"
                it "states the modules a dependent may import" $ do
                    v <- runCat "vapour"
                    modulesOf v `shouldSatisfy` elem "Data.Vapour"
                it "leads with the root module, not an internal one" $ do
                    v <- runCat "vapour"
                    take 1 (modulesOf v) `shouldBe` ["Data.Vapour"]
                it "ranks an internal module below the public ones" $ do
                    v <- runCat "vapour"
                    let ms = modulesOf v
                    ms `shouldSatisfy` \xs ->
                        "Data.Vapour.Internal" `notElem` take 2 xs
                it "carries the import line, so no module name is guessed" $ do
                    v <- runCat "vapour"
                    firstHit v "use" `shouldBe` "import Data.Vapour"
                it "carries the cabal line it must be installed with" $ do
                    v <- runCat "vapour"
                    firstHit v "cabal"
                        `shouldBe` "-- cabal: build-depends: vapour"
                it "says where to read about it" $ do
                    v <- runCat "vapour"
                    firstHit v "homepage"
                        `shouldBe` "https://example.invalid/vapour"
                it "bounds the module list and says how to see the rest" $ do
                    v <- runCat "vapour"
                    length (modulesOf v) `shouldSatisfy` (<= 6)
                    textField "next" v
                        `shouldSatisfy` T.isInfixOf "package=\"vapour\""

            describe "a module only a not-installed package exposes" $ do
                it "resolves to the package that exposes it" $ do
                    v <- runCat "Data.Vapour.Cloud"
                    stateOf v `shouldBe` "found"
                    firstHit v "package" `shouldBe` "vapour"
                it "is absent-known, never installed" $ do
                    v <- runCat "Data.Vapour.Cloud"
                    firstHit v "install" `shouldBe` "absent-known"
                it "carries the import line for the module asked about" $ do
                    v <- runCat "Data.Vapour.Cloud"
                    firstHit v "use" `shouldBe` "import Data.Vapour.Cloud"
                it "carries the cabal line that makes the import work" $ do
                    v <- runCat "Data.Vapour.Cloud"
                    firstHit v "cabal"
                        `shouldBe` "-- cabal: build-depends: vapour"

            describe "scoping to a package the session cannot hold" $ do
                it "lists what the package exposes, rather than denying it" $ do
                    v <- runScoped "vapour" "Data.Vapour.Mist"
                    stateOf v `shouldBe` "found"
                    map (hitText "module") (hitsOf v)
                        `shouldSatisfy` elem "Data.Vapour.Mist"
                it "says the index covers installed packages only" $ do
                    v <- runScoped "vapour" "condense"
                    textField "narrow" v
                        `shouldSatisfy` T.isInfixOf "not installed"
                it "does not read no-coverage as the package lacking the name" $ do
                    v <- runScoped "vapour" "condense"
                    textField "narrow" v
                        `shouldSatisfy` (not . T.isInfixOf "matched none")

            describe "what the facts cache cannot answer" $
                it "a module no package exposes stays not_found" $ do
                    v <- runCat "Data.Frobwizzle.Core"
                    stateOf v `shouldBe` "not_found"

runScoped :: Text -> Text -> IO Value
runScoped pkg q = runCatArgs q (object ["query" .= q, "package" .= pkg])

modulesOf :: Value -> [Text]
modulesOf v = case hitsOf v of
    (Object o : _) -> case KM.lookup (K.fromText "modules") o of
        Just (Array xs) -> [t | String t <- toList xs]
        _ -> []
    _ -> []

firstHit :: Value -> Text -> Text
firstHit v k = case hitsOf v of
    (h : _) -> hitText k h
    [] -> ""
