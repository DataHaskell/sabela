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
    runCatArgsIn,
    stateOf,
    synHackageNames,
    synHoogleCatalogued,
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
              \Data.Vapour.Droplet.Size Data.Vapour.Mist\t1.2.3"
            , "nimbus\t\tRain\tNimbus.Sky"
            , "hodograph\thttps://example.invalid/hodograph\tWind shear plots\t\
              \Data.Hodograph.Internal.Raw Data.Hodograph Data.Hodograph.Plot \
              \Data.Hodograph.Plot.Polar Data.Hodograph.Shear \
              \Data.Hodograph.Shear.Layer Data.Hodograph.Trace \
              \Data.Hodograph.Wind"
            ]
        )
    setEnv "SABELA_HACKAGE_FACTS" path

installBoth :: IO ()
installBoth = do
    installNamesFileWith (synHackageNames ++ ["vapour", "hodograph"])
    installFactsFile

{- | The live shape: Hoogle names the package but describes nothing in it, so
another source has already spoken for it by the time the index is consulted.
-}
runCatalogued :: Text -> IO Value
runCatalogued q =
    runCatArgsIn synHoogleCatalogued q (object ["query" .= q])

-- | The scoped follow-up, where Hoogle has already named the package.
runScopedCatalogued :: Text -> IO Value
runScopedCatalogued pkg =
    runCatArgsIn synHoogleCatalogued "" (object ["package" .= pkg])

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
                it "states the release the index documented" $ do
                    v <- runCat "vapour"
                    firstHit v "version" `shouldBe` "1.2.3"
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

            {- The 2026-08-07 episode: eight scoped searches, every one
            answered "matched none; drop the filter". The module named a
            package the index could describe and nothing asked it which. -}
            describe "scoping to a module only an absent package exposes" $ do
                it "names the package that exposes the scoped module" $ do
                    v <- runModScoped "Data.Vapour.Cloud" "condense"
                    textField "narrow" v `shouldSatisfy` T.isInfixOf "vapour"
                it "says the index covers installed packages only" $ do
                    v <- runModScoped "Data.Vapour.Cloud" "condense"
                    textField "narrow" v
                        `shouldSatisfy` T.isInfixOf "not installed"
                it "carries the cabal line that would make it searchable" $ do
                    v <- runModScoped "Data.Vapour.Cloud" "condense"
                    textField "narrow" v
                        `shouldSatisfy` T.isInfixOf "build-depends: vapour"
                it "does not blame the filter for a package it never consulted" $ do
                    v <- runModScoped "Data.Vapour.Cloud" "condense"
                    textField "narrow" v
                        `shouldSatisfy` (not . T.isInfixOf "drop the filter")
                {- A module scope under an installed package must keep saying
                what it says now; this note is for the absent case only. -}
                it "says nothing of the kind for a module already in scope" $ do
                    v <- runModScoped "Data.Map" "condense"
                    textField "narrow" v
                        `shouldSatisfy` (not . T.isInfixOf "not installed")

                {- Listing the indexes as consulted implies they could have
                answered. For a package none of them covers they could not, and
                the caller who reads it as "no such name" varies the query. -}
                it "does not claim the indexes could have answered" $ do
                    v <- runModScoped "Data.Vapour.Cloud" "condense"
                    textField "next" v
                        `shouldSatisfy` (not . T.isInfixOf "No match for")
                it "names the install that would let them answer" $ do
                    v <- runModScoped "Data.Vapour.Cloud" "condense"
                    textField "next" v
                        `shouldSatisfy` T.isInfixOf "build-depends: vapour"
                it "still names the sources when the miss is a real miss" $ do
                    v <- runCat "zzznotathing"
                    textField "next" v
                        `shouldSatisfy` T.isInfixOf "No match for"

            {- Hoogle holds installed haddock only, so for an absent package it
            names it and describes nothing. That must not displace the index,
            the only source that can describe the package at all. -}
            describe "when Hoogle has already named the package" $ do
                it "still states the modules a dependent may import" $ do
                    v <- runCatalogued "hodograph"
                    modulesOf v `shouldSatisfy` elem "Data.Hodograph"
                it "still leads with the root module" $ do
                    v <- runCatalogued "hodograph"
                    take 1 (modulesOf v) `shouldBe` ["Data.Hodograph"]
                it "still carries the import line" $ do
                    v <- runCatalogued "hodograph"
                    firstHit v "use" `shouldBe` "import Data.Hodograph"
                it "still says where to read about it" $ do
                    v <- runCatalogued "hodograph"
                    firstHit v "homepage"
                        `shouldBe` "https://example.invalid/hodograph"
                it "names the module on the hit, not just in the list" $ do
                    v <- runCatalogued "hodograph"
                    firstHit v "module" `shouldBe` "Data.Hodograph"
                it "does not claim the package is installed" $ do
                    v <- runCatalogued "hodograph"
                    firstHit v "install" `shouldBe` "absent-known"

            {- Two notes tell the caller a follow-up lists the rest. Returning
            the same leading modules to it makes the answer a loop. -}
            describe "the follow-up the leading answer promises" $ do
                it "names the scoped call" $ do
                    v <- runCat "vapour"
                    textField "next" v
                        `shouldSatisfy` T.isInfixOf "package=\"vapour\""
                it "delivers more of the structure than the lead did" $ do
                    lead <- runCat "vapour"
                    whole <- runScoped "vapour" ""
                    length (modulesOf whole)
                        `shouldSatisfy` (> length (modulesOf lead))
                it "reaches a module the lead had to leave out" $ do
                    lead <- runCat "vapour"
                    whole <- runScoped "vapour" ""
                    let gained =
                            [m | m <- modulesOf whole, m `notElem` modulesOf lead]
                    gained `shouldSatisfy` not . null
                it "does not collapse a single-root package to one row" $ do
                    v <- runScopedCatalogued "hodograph"
                    length (modulesOf v) `shouldSatisfy` (> 1)
                it "reaches past the depth the lead ranking stops at" $ do
                    v <- runScopedCatalogued "hodograph"
                    modulesOf v `shouldSatisfy` elem "Data.Hodograph.Shear.Layer"

            {- A module filter the index cannot match exactly is not thereby
            unknown: the fragment names a namespace the index does hold, and
            saying so beats sending the caller back to guess. -}
            describe "a module fragment no package exposes exactly" $ do
                it "resolves a distinctive fragment to the real module" $ do
                    v <- runCat "Hodograph"
                    stateOf v `shouldBe` "found"
                    map (hitText "module") (hitsOf v)
                        `shouldSatisfy` any (T.isPrefixOf "Data.Hodograph")
                it "names the package that exposes it" $ do
                    v <- runCat "Hodograph"
                    map (hitText "package") (hitsOf v)
                        `shouldSatisfy` elem "hodograph"
                it "leaves an exact module match untouched" $ do
                    v <- runCat "Data.Vapour.Cloud"
                    firstHit v "module" `shouldBe` "Data.Vapour.Cloud"
                it "does not engage on a fragment too short to mean anything" $ do
                    v <- runCat "Ho"
                    stateOf v `shouldBe` "not_found"

            describe "what the facts cache cannot answer" $
                it "a module no package exposes stays not_found" $ do
                    v <- runCat "Data.Frobwizzle.Core"
                    stateOf v `shouldBe` "not_found"

runScoped :: Text -> Text -> IO Value
runScoped pkg q = runCatArgs q (object ["query" .= q, "package" .= pkg])

-- | Scoped to a module rather than a package, as the live episode was.
runModScoped :: Text -> Text -> IO Value
runModScoped m q = runCatArgs q (object ["query" .= q, "module" .= m])

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
