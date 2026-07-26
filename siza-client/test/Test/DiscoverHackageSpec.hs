{-# LANGUAGE OverloadedStrings #-}

{- | The @hackage@ source's name lookups over @data/hackage-packages.txt@.
Hackage names are mixed case; a caller's query is not, so every lookup here
has to be case-insensitive on BOTH sides while still reporting the index's
own spelling — that is what a @-- cabal: build-depends:@ line needs.
-}
module Test.DiscoverHackageSpec (discoverHackageSpec) where

import Control.Exception (bracket)
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
import Siza.Agent.Discover.Classify (notebookAnswer)
import Siza.Agent.Discover.Hackage (hackageInfoFor, hackageMatching)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    NotebookEnv (..),
    SourceAnswer (..),
 )
import Siza.Agent.Tools (catalogueWith)

{- | Run against a fixture index. The names are real Hackage spellings: the
capitalised ones are what the asymmetric lowercasing used to lose.
-}
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

discoverHackageSpec :: Spec
discoverHackageSpec = do
    mcpSurfaceSpec
    notebookSourceSpec
    hackageNameSpec

env0 :: NotebookEnv
env0 = NotebookEnv [] [] [] [] [] []

hackageNameSpec :: Spec
hackageNameSpec = describe "the hackage name source" $ do
    describe "hackageMatching (keyword -> upstream candidates)" $ do
        {- live_test35_wine: `frames` returned one unrelated session hit and
        no package at all, though both Frames and dataframe were in the index. -}
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

{- | live_test37 asked its own EMPTY notebook for `DataFrame`, `dataset`,
`wine`, `Data.Csv` and `mean`, read five misses as evidence, and hand-rolled
the work. Routing "where is X?" to a notebook tool or a library tool asks the
caller to know the answer first, so discover answers both.
-}
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

{- | The first harness-free probe: tools/list advertised discover, tools/call
answered "unknown tool: discover" five times — the call path gated on
parseToolName before the agent dispatcher that owns discover. Every
advertised tool must be callable; the two lists come from one catalogue, so
the invariant is checkable without a server.
-}
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
        -- The property the bug violated: a name the server does not parse
        -- must still be a name the agent dispatcher routes.
        parseToolName "discover" `shouldBe` Nothing
