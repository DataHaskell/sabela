{-# LANGUAGE OverloadedStrings #-}

module Test.FilesSpec (spec) where

import Control.Monad (void)
import System.Directory (
    createDirectoryIfMissing,
    emptyPermissions,
    getPermissions,
    setPermissions,
 )
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.AI.Artefact (Artefact (..), ArtefactView (..))
import Sabela.AI.Files (
    FileEntry (..),
    ReadError (..),
    listLocal,
    readLocal,
    readMissCandidates,
 )

writeFixture :: FilePath -> FilePath -> String -> IO ()
writeFixture root rel body = do
    let full = root </> rel
    createDirectoryIfMissing True (takeDirectory full)
    writeFile full body

spec :: Spec
spec = describe "Sabela.AI.Files" $ do
    describe "listLocal" $ do
        it "lists every file under the work dir when no prefix is given" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "examples/data/iris.parquet" "x"
                writeFixture root "notes.md" "y"
                (got, total) <- listLocal root ""
                map feName got
                    `shouldMatchList` ["examples/data/iris.parquet", "notes.md"]
                total `shouldBe` 2

        it "restricts the listing to the requested subtree" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "examples/data/iris.parquet" "x"
                writeFixture root "notes.md" "y"
                (got, _) <- listLocal root "examples"
                map feName got `shouldBe` ["examples/data/iris.parquet"]

        it "treats a leading dot-slash as the work dir root" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "notes.md" "y"
                (got, _) <- listLocal root "./"
                map feName got `shouldBe` ["notes.md"]

        it "lists nothing for a prefix that climbs out of the work dir" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "inner/notes.md" "y"
                writeFixture root "outside.md" "x"
                (got, total) <- listLocal (root </> "inner") ".."
                (map feName got, total) `shouldBe` ([], 0)

        it "skips dot directories" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root ".git/config" "x"
                writeFixture root "notes.md" "y"
                (got, _) <- listLocal root ""
                map feName got `shouldBe` ["notes.md"]

    describe "readLocal" $ do
        it "reads a file under the work dir" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "notes.md" "hello"
                got <- readLocal root "notes.md"
                case got of
                    Left e -> expectationFailure ("expected a read, got " <> show e)
                    Right a -> do
                        arText a `shouldBe` "hello"
                        arBytes a `shouldBe` 5
                        arTruncated a `shouldBe` False
                        arView a `shouldBe` TextView

        it "reports a missing file as NotFound" $
            withSystemTempDirectory "files" $ \root -> do
                got <- readLocal root "nope.md"
                void got `shouldBe` Left NotFound

        it "refuses to escape the work dir" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "inner/notes.md" "hello"
                got <- readLocal (root </> "inner") "../../etc/hosts"
                void got `shouldBe` Left OutsideWorkDir

    describe "a miss carries the nearest candidates" $ do
        it "ranks the same-stem file above the places it lies in" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "examples/data/iris.parquet" "x"
                (got, searched) <- readMissCandidates root "./examples/data/iris.csv"
                take 1 got `shouldBe` ["examples/data/iris.parquet"]
                searched `shouldBe` 3

        it "names the same file under its real directory" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "examples/data/housing.csv" "x"
                (got, _) <- readMissCandidates root "./data/housing.csv"
                got `shouldBe` ["examples/data/housing.csv"]

        it "names a directory of that name, which no file search can find" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "examples/data/housing.csv" "x"
                (got, _) <- readMissCandidates root "./elsewhere/data"
                got `shouldBe` ["examples/data"]

        it "is empty when nothing on disk resembles the path" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "notes.md" "x"
                (got, searched) <- readMissCandidates root "./nope.csv"
                got `shouldBe` []
                searched `shouldBe` 1

    describe "a path that is there but is not a file" $ do
        it "is a directory, not a miss" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "examples/data/housing.csv" "x"
                got <- readLocal root "examples/data"
                void got `shouldBe` Left IsDirectory

        it "is unread, not missing, when its bytes cannot be opened" $
            withSystemTempDirectory "files" $ \root -> do
                writeFixture root "shut.dat" "x"
                perms <- getPermissions (root </> "shut.dat")
                setPermissions (root </> "shut.dat") emptyPermissions
                got <- readLocal root "shut.dat"
                setPermissions (root </> "shut.dat") perms
                case got of
                    Right _ -> pendingWith "this runner can open a file with no permissions"
                    Left e -> e `shouldBe` NotReadable
