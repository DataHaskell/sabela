{-# LANGUAGE OverloadedStrings #-}

{- | File-not-found near-miss repair (C4-1): a wrong path GHC's runtime
raised on IS a fact once a real file under the work dir near-misses it —
these pin the parse, the retry, and the "ask the user" fallback.
-}
module Test.PathRepairSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.PathRepair (
    notFoundPath,
    pathNearMissFix,
    pathNotFoundGuidance,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Diagnose (Guidance (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

raising :: Text -> Either Text ExecutionResult
raising msg = Right (ExecutionResult [] (Just msg) [] [])

doesNotExist :: FilePath -> Text
doesNotExist p =
    "*** Exception: "
        <> T.pack p
        <> ": openFile: does not exist (No such file or directory)"

writeFixture :: FilePath -> FilePath -> IO ()
writeFixture root rel = do
    let full = root </> rel
    createDirectoryIfMissing True (takeDirectoryOf full)
    writeFile full "x"
  where
    takeDirectoryOf f = reverse (dropWhile (/= '/') (reverse f))

spec :: Spec
spec = describe "Sabela.AI.PathRepair" $ do
    describe "notFoundPath" $ do
        it "extracts the path from a GHCi runtime does-not-exist exception" $
            notFoundPath (doesNotExist "/examples/data/housing.csv")
                `shouldBe` Just "/examples/data/housing.csv"

        it "is Nothing for an error with no does-not-exist line" $
            notFoundPath "Couldn't match expected type `Int'" `shouldBe` Nothing

        it "is Nothing for an unrelated does-not-exist-shaped line" $
            notFoundPath "the database does not exist" `shouldBe` Nothing

        {- live_test33_wine: a getAddrInfo DNS failure also ends in "does not
        exist", and reading the reason off the whole line surfaced the failed
        host lookup as a missing file. -}
        it "is Nothing for a DNS failure that merely ends in does not exist" $
            notFoundPath dnsFailure `shouldBe` Nothing

        it "still reads a genuine path whose reason field follows the location" $
            notFoundPath (doesNotExist "./data/wine.csv")
                `shouldBe` Just "./data/wine.csv"

    describe "pathNearMissFix" $ do
        it "retries a leading-dot-dropped path against its unique basename match" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "examples/data/housing.csv"
                got <-
                    pathNearMissFix
                        root
                        (raising (doesNotExist "/examples/data/housing.csv"))
                        "df <- D.readCsv \"/examples/data/housing.csv\""
                got
                    `shouldBe` Just
                        "df <- D.readCsv \"./examples/data/housing.csv\""

        it "retries a directory typo against its unique basename match" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "data/housing.csv"
                got <-
                    pathNearMissFix
                        root
                        (raising (doesNotExist "./examples/housing.csv"))
                        "readCsv \"./examples/housing.csv\""
                got `shouldBe` Just "readCsv \"./data/housing.csv\""

        it "does not retry when two files share the failing basename" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "a/housing.csv"
                writeFixture root "b/housing.csv"
                got <-
                    pathNearMissFix
                        root
                        (raising (doesNotExist "/housing.csv"))
                        "readCsv \"/housing.csv\""
                got `shouldBe` Nothing

        it "does not retry when no file under the work dir is close" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "notes.md"
                got <-
                    pathNearMissFix
                        root
                        (raising (doesNotExist "/nope.csv"))
                        "readCsv \"/nope.csv\""
                got `shouldBe` Nothing

        it "is Nothing on a failure that is not a file-not-found" $
            withSystemTempDirectory "path-repair" $ \root -> do
                got <-
                    pathNearMissFix
                        root
                        (raising "Couldn't match expected type `Int'")
                        "x = (1 :: Bool)"
                got `shouldBe` Nothing

    describe "pathNotFoundGuidance" $ do
        it "is Nothing when the path uniquely resolves (no need to ask)" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "examples/data/housing.csv"
                got <-
                    pathNotFoundGuidance
                        root
                        (raising (doesNotExist "/examples/data/housing.csv"))
                got `shouldBe` Nothing

        it "lists the nearest candidates when the basename is ambiguous" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "a/housing.csv"
                writeFixture root "b/housing.csv"
                Just g <-
                    pathNotFoundGuidance root (raising (doesNotExist "/housing.csv"))
                gCategory g `shouldBe` "file-not-found"
                forM_ ["a/housing.csv", "b/housing.csv"] $ \c ->
                    gMessage g `shouldSatisfy` (T.pack c `T.isInfixOf`)

        it "says plainly that nothing was found when there is no close match" $
            withSystemTempDirectory "path-repair" $ \root -> do
                writeFixture root "notes.md"
                Just g <- pathNotFoundGuidance root (raising (doesNotExist "/nope.csv"))
                gCategory g `shouldBe` "file-not-found"
                gMessage g `shouldSatisfy` ("no similar file" `T.isInfixOf`)

        it "is Nothing on a failure that is not a file-not-found" $ do
            got <-
                pathNotFoundGuidance
                    "."
                    (raising "Couldn't match expected type `Int'")
            got `shouldBe` Nothing

    {- live_test35_wine: the cell called readFile on a URL. "No similar file
    was found under the work dir. Ask the user for the correct path" is both
    useless and wrong — the user never supplied a path. -}
    describe "a URL passed where a path was expected" $ do
        it "is reported as a URL, not as a missing file" $
            withSystemTempDirectory "path-repair" $ \root -> do
                got <- pathNotFoundGuidance root (Left (doesNotExist wineUrl))
                fmap gCategory got `shouldBe` Just "url-as-path"

        it "never tells the caller to ask the user for a path" $
            withSystemTempDirectory "path-repair" $ \root -> do
                got <- pathNotFoundGuidance root (Left (doesNotExist wineUrl))
                fmap gMessage got
                    `shouldSatisfy` maybe False (not . T.isInfixOf "Ask the user")

        it "keeps the file-not-found category for a real relative path" $
            withSystemTempDirectory "path-repair" $ \root -> do
                got <- pathNotFoundGuidance root (Left (doesNotExist "./nope.csv"))
                fmap gCategory got `shouldBe` Just "file-not-found"

wineUrl :: FilePath
wineUrl = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

-- | The getAddrInfo failure exactly as live_test33_wine received it.
dnsFailure :: Text
dnsFailure =
    "*** Exception: HttpExceptionRequest Request { host = \"archive.uci.edu\" } \
    \(ConnectionFailure Network.Socket.getAddrInfo (called with preferred \
    \socket type/protocol: AddrInfo {addrFlags = [AI_ADDRCONFIG]}, host name: \
    \\"archive.uci.edu\", service name: \"443\"): does not exist (nodename nor \
    \servname provided, or not known))"
