{-# LANGUAGE OverloadedStrings #-}

module Test.DiscoverHackageSpec (discoverHackageSpec) where

import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Sabela.AI.Capabilities.ToolName (parseToolName)
import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.Classify (notebookAnswer)
import Siza.Agent.Discover.Hackage (
    hackageFactsFor,
    hackageInfoFor,
    hackageMatching,
    hackageModuleOwners,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Render (hackageJson)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    SourceAnswer (..),
 )
import Siza.Agent.Tools (catalogueWith)
import Test.LadderFixtures (hodatimeFactsRow)

withNames :: IO a -> IO a
withNames act = bracket acquire release (const act)
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let dir = tmp </> "siza-hackage-names-spec"
        createDirectoryIfMissing True dir
        let path = dir </> "names.txt"
        TIO.writeFile path "Frames\ndataframe\nChart\ncassava\nHUnit\n"
        setEnv "SABELA_HACKAGE_NAMES" path
        pure dir
    release dir = unsetEnv "SABELA_HACKAGE_NAMES" >> removeDirectoryRecursive dir

withFacts :: IO a -> IO a
withFacts act = bracket acquire release (const act)
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let dir = tmp </> "siza-hackage-facts-spec"
        createDirectoryIfMissing True dir
        let path = dir </> "facts.tsv"
        TIO.writeFile
            path
            ( T.unlines
                [ hodatimeFactsRow
                , "cassava\t\tCSV parsing\tData.Csv Data.Csv.Streaming"
                , "bare\t\t\t"
                , "widgets-a\t\tWidgets\tWeb.Widget.Types"
                , "widgets-b\t\tWidgets\tUi.Widget.Types"
                , "widgets-c\t\tWidgets\tApp.Widget.Types"
                , "widgets-d\t\tWidgets\tLib.Widget.Types"
                ]
            )
        setEnv "SABELA_HACKAGE_FACTS" path
        pure dir
    release dir = unsetEnv "SABELA_HACKAGE_FACTS" >> removeDirectoryRecursive dir

discoverHackageSpec :: Spec
discoverHackageSpec = do
    mcpSurfaceSpec
    notebookSourceSpec
    hackageNameSpec
    hackageFactsSpec
    namesLadderSpec

{- | A temporary XDG data home, optionally holding the names mirror the cache
refresh writes. Guards the rung 'loadHackageNames' reaches when no operator
override is set and the CWD has no @data/@ directory.
-}
withXdgData :: Bool -> (FilePath -> IO a) -> IO a
withXdgData mirror act = bracket acquire release body
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let dir = tmp </> "siza-xdg-data-spec"
        createDirectoryIfMissing True (dir </> "sabela")
        when mirror $
            TIO.writeFile
                (dir </> "sabela" </> "hackage-packages.txt")
                "cassava\nhodatime\n"
        unsetEnv "SABELA_HACKAGE_NAMES"
        setEnv "SABELA_DATA_DIR" (dir </> "nodata")
        setEnv "XDG_DATA_HOME" dir
        pure dir
    release dir = do
        unsetEnv "XDG_DATA_HOME"
        unsetEnv "SABELA_DATA_DIR"
        removeDirectoryRecursive dir
    body dir = act (dir </> "sabela" </> "hackage-packages.txt")

namesLadderSpec :: Spec
namesLadderSpec = describe "where the names cache is looked for" $ do
    it "reaches the machine-local mirror when no override is set" $
        withXdgData True $ \_ -> do
            info <- hackageInfoFor ["cassava"]
            hiAvailable info `shouldBe` True
            hiKnown info `shouldBe` ["cassava"]

    it "an operator override wins even when it names a missing file" $
        withXdgData True $ \_ -> do
            setEnv "SABELA_HACKAGE_NAMES" "/nonexistent/names.txt"
            info <- hackageInfoFor ["cassava"]
            unsetEnv "SABELA_HACKAGE_NAMES"
            hiAvailable info `shouldBe` False
            hiChecked info `shouldBe` ["/nonexistent/names.txt"]

    it "a miss reports every path it checked" $
        withXdgData False $ \mirrorPath -> do
            tmp <- getTemporaryDirectory
            let dataDir = tmp </> "siza-xdg-data-spec" </> "nodata"
            info <- hackageInfoFor ["cassava"]
            hiAvailable info `shouldBe` False
            hiChecked info
                `shouldBe` [ T.pack (dataDir </> "hackage-packages.txt")
                           , T.pack mirrorPath
                           ]

    it "the facts cache reads the mirror through the same ladder" $
        withXdgData False $ \_ -> do
            tmp <- getTemporaryDirectory
            let dir = tmp </> "siza-xdg-data-spec" </> "sabela"
            TIO.writeFile
                (dir </> "hackage-facts.tsv")
                "cassava\t\tCSV parsing\tData.Csv\n"
            unsetEnv "SABELA_HACKAGE_FACTS"
            fs <- hackageFactsFor ["cassava"]
            map (pfModules . snd) fs `shouldBe` [["Data.Csv"]]

    describe "the unavailable note" $ do
        it "names the paths that were checked" $ do
            let v = hackageJson (HackageInfo False [] [] ["/a/n.txt", "/b/n.txt"])
            noteOf v `shouldSatisfy` T.isInfixOf "/a/n.txt"
            noteOf v `shouldSatisfy` T.isInfixOf "/b/n.txt"
            noteOf v `shouldSatisfy` T.isInfixOf "make search-cache"
        it "an available index carries no note" $
            case hackageJson (HackageInfo True [] [] []) of
                Object o -> KM.lookup "note" o `shouldBe` Nothing
                v -> expectationFailure ("not an object: " <> show v)
  where
    noteOf v = case v of
        Object o | Just (String n) <- KM.lookup "note" o -> n
        _ -> ""

hackageFactsSpec :: Spec
hackageFactsSpec = describe "the hackage facts source" $ do
    describe "what a not-installed package states about itself" $ do
        it "carries the modules a dependent may import" $ do
            fs <- withFacts (hackageFactsFor ["hodatime"])
            map (pfModules . snd) fs
                `shouldBe` [
                               [ "Data.HodaTime"
                               , "Data.HodaTime.Instant"
                               , "Data.HodaTime.Duration"
                               , "Data.HodaTime.Compat"
                               , "Data.HodaTime.Calendar.Gregorian"
                               ]
                           ]
        it "carries where to read about it" $ do
            fs <- withFacts (hackageFactsFor ["hodatime"])
            map (pfHomepage . snd) fs
                `shouldBe` ["https://example.invalid/hodatime"]
        it "states nothing for a package the index does not hold" $
            withFacts (hackageFactsFor ["nosuchpkg"]) `shouldReturn` []
        it "reads a row that states only a name" $ do
            fs <- withFacts (hackageFactsFor ["bare"])
            map (pfModules . snd) fs `shouldBe` [[]]

    describe "which package exposes a module" $ do
        it "names the package a module is imported from" $ do
            os <- withFacts (hackageModuleOwners "Data.HodaTime")
            map fst os `shouldBe` ["hodatime"]
        it "answers for a module below the root too" $ do
            os <- withFacts (hackageModuleOwners "Data.Csv.Streaming")
            map fst os `shouldBe` ["cassava"]
        it "does not answer for a module no package exposes" $
            withFacts (hackageModuleOwners "Data.Nonesuch") `shouldReturn` []
        it "does not answer for a prefix that is not itself a module" $
            withFacts (hackageModuleOwners "Data") `shouldReturn` []
        it "reads a fragment of a distinguishing component" $ do
            os <- withFacts (hackageModuleOwners "Hoda")
            map fst os `shouldBe` ["hodatime"]
        it "answers with the real module names, never the fragment" $ do
            os <- withFacts (hackageModuleOwners "Hoda")
            concatMap (pfModules . snd) os
                `shouldSatisfy` all (T.isPrefixOf "Data.HodaTime")
        it "says nothing for a fragment too many packages share" $
            withFacts (hackageModuleOwners "Widget") `shouldReturn` []
        it "still says nothing for a fragment too short to mean anything" $
            withFacts (hackageModuleOwners "Ho") `shouldReturn` []

    describe "an absent facts cache" $
        it "states no facts rather than denying the package" $ do
            unsetEnv "SABELA_HACKAGE_FACTS"
            setEnv "SABELA_HACKAGE_FACTS" "/nonexistent/facts.tsv"
            fs <- hackageFactsFor ["cassava"]
            unsetEnv "SABELA_HACKAGE_FACTS"
            fs `shouldBe` []

env0 :: NotebookEnv
env0 = NotebookEnv [] [] [] [] [] []

hackageNameSpec :: Spec
hackageNameSpec = describe "the hackage name source" $ do
    describe "hackageMatching (keyword -> upstream candidates)" $ do
        it "reaches a capitalised package from a lowercase token" $
            withNames (hackageMatching 10 ["frames"])
                `shouldReturn` ["Frames"]

        it "still reaches a lowercase package" $
            withNames (hackageMatching 10 ["cassava"])
                `shouldReturn` ["cassava"]

        it "matches a token inside a longer name, either case" $
            withNames (hackageMatching 10 ["frame"])
                `shouldReturn` ["Frames", "dataframe"]

        it "ignores tokens under three characters" $
            withNames (hackageMatching 10 ["ch"]) `shouldReturn` []

        it "respects the cap" $
            withNames (fmap length (hackageMatching 1 ["frame"]))
                `shouldReturn` 1

    describe "hackageInfoFor (does this package exist upstream)" $ do
        it "reports the index's own spelling, not the caller's" $
            withNames (fmap hiKnown (hackageInfoFor ["frames"]))
                `shouldReturn` ["Frames"]

        it "keeps an exactly-spelled name unchanged" $
            withNames (fmap hiKnown (hackageInfoFor ["cassava"]))
                `shouldReturn` ["cassava"]

        it "reports absence for a name in no case" $
            withNames (fmap hiKnown (hackageInfoFor ["nosuchpkg"]))
                `shouldReturn` []

        it "an unavailable index is unavailability, never absence" $ do
            unsetEnv "SABELA_HACKAGE_NAMES"
            setEnv "SABELA_HACKAGE_NAMES" "/nonexistent/names.txt"
            info <- hackageInfoFor ["cassava"]
            unsetEnv "SABELA_HACKAGE_NAMES"
            hiAvailable info `shouldBe` False

notebookSourceSpec :: Spec
notebookSourceSpec = describe "cell matches are a discover source" $ do
    let interp = interpret env0 "helper"
        matches ms = Just (object ["matches" .= ms])
    it "turns a matched cell into a notebook-attributed hit" $ do
        let hits = saHits (notebookAnswer interp (matches [object ["cellId" .= (3 :: Int)]]))
        map dhInstall hits `shouldBe` [InstNotebook]
        map dhUse hits `shouldBe` [Just "defined in notebook cell 3"]
    it "names the searched thing, so it ranks beside library hits" $ do
        let hits = saHits (notebookAnswer interp (matches [object ["cellId" .= (1 :: Int)]]))
        map dhName hits `shouldBe` ["helper"]
    it "an empty notebook contributes no hits, and no claim" $
        saHits (notebookAnswer interp (matches ([] :: [Value]))) `shouldBe` []
    it "an absent notebook answer is not a denial" $
        saHits (notebookAnswer interp Nothing) `shouldBe` []

mcpSurfaceSpec :: Spec
mcpSurfaceSpec = describe "every advertised MCP tool is dispatchable" $
    it "discover is on the agent surface (not a server ToolName)" $ do
        let names =
                [ n
                | Object o <- catalogueWith False
                , Just (Object f) <- [KM.lookup "function" o]
                , Just (String n) <- [KM.lookup "name" f]
                ]
        names `shouldSatisfy` elem "discover"
        parseToolName "discover" `shouldBe` Nothing
