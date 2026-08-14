{-# LANGUAGE OverloadedStrings #-}

module Test.HackageFactsSpec (spec) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (addUTCTime)
import System.Directory (
    createDirectoryIfMissing,
    getModificationTime,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    setModificationTime,
 )
import System.FilePath ((</>))
import Test.Hspec

import Sabela.AI.FactsRow (PkgFacts (..), parseFactsRow, renderFactsRow)
import Sabela.AI.HackageFacts (factsVersion, moduleOwners)
import Test.WorldFixtures (hodatimeFactsRow, withEnvVars)

factsRows :: Text
factsRows = T.unlines [hodatimeFactsRow, "cassava\t\tCSV\tData.Csv\t"]

-- | The hodatime row restated with a newer release, for the cache cases.
rebumpedRows :: Text
rebumpedRows = case parseFactsRow hodatimeFactsRow of
    Just (n, f) -> renderFactsRow n f{pfVersion = "9.9.9"} <> "\n"
    Nothing -> ""

withFactsWorld :: (FilePath -> IO a) -> IO a
withFactsWorld act = bracket acquire release body
  where
    acquire = do
        tmp <- getTemporaryDirectory
        let dir = tmp </> "sabela-hackage-facts-spec"
        createDirectoryIfMissing True dir
        let path = dir </> "facts.tsv"
        TIO.writeFile path factsRows
        pure (dir, path)
    release (dir, _) = removeDirectoryRecursive dir
    body (_, path) =
        withEnvVars [("SABELA_HACKAGE_FACTS", path)] (act path)

withFactsFile :: IO a -> IO a
withFactsFile act = withFactsWorld (const act)

spec :: Spec
spec = describe "the server-side Hackage facts index" $ do
    it "states the release the index documented" $
        withFactsFile (factsVersion "hodatime") `shouldReturn` Just "0.2.2.1"
    it "states no version for a pre-version row" $
        withFactsFile (factsVersion "cassava") `shouldReturn` Nothing
    it "states nothing for an unknown package" $
        withFactsFile (factsVersion "nosuchpkg") `shouldReturn` Nothing
    it "names the package exposing a module, exactly" $ do
        os <- withFactsFile (moduleOwners "Data.HodaTime.Instant")
        map fst os `shouldBe` ["hodatime"]
        map (pfVersion . snd) os `shouldBe` ["0.2.2.1"]
    it "does not answer for a module nothing exposes" $
        withFactsFile (moduleOwners "Data.Nonesuch") `shouldReturn` []
    it "a missing file answers empty, never throws" $ do
        r <-
            withEnvVars
                [("SABELA_HACKAGE_FACTS", "/nonexistent/facts.tsv")]
                (factsVersion "hodatime")
        r `shouldBe` Nothing
    it "a rewritten file with a newer stamp is re-read" $
        withFactsWorld $ \path -> do
            factsVersion "hodatime" `shouldReturn` Just "0.2.2.1"
            stamp <- getModificationTime path
            TIO.writeFile path rebumpedRows
            setModificationTime path (addUTCTime 1 stamp)
            factsVersion "hodatime" `shouldReturn` Just "9.9.9"
    it "an unchanged stamp still answers from the cached map" $
        withFactsWorld $ \path -> do
            stamp <- getModificationTime path
            setModificationTime path stamp
            factsVersion "hodatime" `shouldReturn` Just "0.2.2.1"
            TIO.writeFile path rebumpedRows
            setModificationTime path stamp
            factsVersion "hodatime" `shouldReturn` Just "0.2.2.1"
