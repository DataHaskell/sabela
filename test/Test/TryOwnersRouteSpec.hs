{-# LANGUAGE OverloadedStrings #-}

{- | The owners route for a trial's not-found module: the Hackage facts name
every package exposing it, and the trial walks them in order under the owner
cap, landing on the first that compiles or keeping the original miss.
-}
module Test.TryOwnersRouteSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.Server (newApp)
import Sabela.Session.Materialize (CandidateSpec (..), DisposableResult)
import Test.TrySeamFixtures (field, green, red, renderAll, textField, tryWith)
import Test.WorldFixtures (withEnvVars)

{- | The Hackage facts pinned to the given rows, one package per line. The
repairs under test must find each owner through the facts, not a store.
-}
withFacts :: [Text] -> IO a -> IO a
withFacts rows act =
    withSystemTempDirectory "sabela-facts" $ \dir -> do
        let path = dir </> "hackage-facts.tsv"
        TIO.writeFile path (T.unlines rows)
        withEnvVars [("SABELA_HACKAGE_FACTS", path)] act

splitFactsRow :: Text
splitFactsRow =
    "split\thttps://example.invalid\tsplitting lists\t\
    \Data.List.Split Data.List.Split.Internals\t0.2.5"

-- | One owner row per package name, all exposing the same fictional module.
fanoutRow :: Text -> Text
fanoutRow pkg = pkg <> "\thttps://example.invalid\tfanout\tZubu.Fanout\t1.0"

fanoutSrc :: Text
fanoutSrc = "import Zubu.Fanout (fan)\nfanned = fan 1"

zubuMiss :: DisposableResult
zubuMiss =
    red
        "<no location info>: error:\n\
        \    Could not find module \8216Zubu.Fanout\8217"

splitMiss :: DisposableResult
splitMiss =
    red
        "<no location info>: error:\n\
        \    Could not find module \8216Data.List.Split\8217"

declares :: Text -> CandidateSpec -> Bool
declares pkg s =
    ("build-depends: " <> pkg) `T.isInfixOf` candidateMetadataSource s

spec :: Spec
spec = describe "a not-found module is repaired from the Hackage facts" $ do
    app <- runIO (newApp "." Set.empty Nothing Nothing [])

    it "declares the owning package and keeps the module name (join-fanout)" $
        withFacts [splitFactsRow] $ do
            let src =
                    "import Data.List.Split (splitOn)\n\
                    \splitOn \",\" \"a,b\"" ::
                        Text
                answer s
                    | declares "split" s = green
                    | otherwise = splitMiss
            (_, v) <- tryWith app answer src
            let autofix = textField "autofix" v
            autofix
                `shouldSatisfy` maybe
                    False
                    (T.isInfixOf "build-depends: split")
            autofix
                `shouldSatisfy` maybe False (T.isInfixOf "Data.List.Split")
            textField "outcome" v `shouldBe` Just "ok"

    it "walks past a red owner and lands on the one that compiles" $
        withFacts (map fanoutRow ["fanout-a", "fanout-b"]) $ do
            let answer s
                    | declares "fanout-b" s = green
                    | otherwise = zubuMiss
            (_, v) <- tryWith app answer fanoutSrc
            textField "outcome" v `shouldBe` Just "ok"
            textField "autofix" v
                `shouldSatisfy` maybe
                    False
                    (T.isInfixOf "build-depends: fanout-b")

    it "tries no more owners than the candidate cap admits"
        $ withFacts
            ( map
                fanoutRow
                ["fanout-a", "fanout-b", "fanout-c", "fanout-d", "fanout-e"]
            )
        $ do
            (specs, _) <- tryWith app (const zubuMiss) fanoutSrc
            let tried pkg = any (declares pkg) specs
            map tried ["fanout-a", "fanout-b", "fanout-c"]
                `shouldBe` [True, True, True]
            map tried ["fanout-d", "fanout-e"]
                `shouldBe` [False, False]

    it "keeps the original miss when every owner stays red" $
        withFacts (map fanoutRow ["fanout-a", "fanout-b"]) $ do
            (_, v) <- tryWith app (const zubuMiss) fanoutSrc
            field "autofix" v `shouldBe` Nothing
            renderAll v `shouldSatisfy` T.isInfixOf "Could not find module"
