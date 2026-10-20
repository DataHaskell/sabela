module Test.ReplProjectDirSpec (spec) where

import Data.Maybe (isJust, isNothing)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
 )
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Sabela.State.ReplProject (claimIn, replProjectName)

spec :: Spec
spec = describe "Sabela.State.ReplProject" $ do
    it "names a work dir stably, by basename then path hash" $ do
        replProjectName "/a/notes" `shouldBe` replProjectName "/a/notes"
        replProjectName "/a/notes" `shouldNotBe` replProjectName "/b/notes"
        take 6 (replProjectName "/a/notes") `shouldBe` "notes-"

    it "gives the cache dir to one server and the temp dir to a second" $
        withSystemTempDirectory "repl-cache" $ \root -> do
            let fallback = ("/tmp/x/repl-project", Nothing)
                work = "/home/u/notebooks"
            (dir1, h1) <- claimIn fallback work root
            dir1 `shouldBe` root </> replProjectName work
            h1 `shouldSatisfy` isJust
            (dir2, h2) <- claimIn fallback work root
            dir2 `shouldBe` fst fallback
            h2 `shouldSatisfy` isNothing
            mapM_ hClose h1
            (dir3, h3) <- claimIn fallback work root
            dir3 `shouldBe` dir1
            mapM_ hClose h3

    it "sweeps stale cell modules but keeps build caches on claim" $
        withSystemTempDirectory "repl-cache" $ \root -> do
            let work = "/home/u/notebooks"
                dir = root </> replProjectName work
            createDirectoryIfMissing True (dir </> "dist-newstyle")
            createDirectoryIfMissing True (dir </> "Cells")
            writeFile (dir </> "Cells" </> "Old.hs") ""
            writeFile (dir </> "Main.hs") ""
            (_, h) <- claimIn ("/unused", Nothing) work root
            mapM_ hClose h
            doesDirectoryExist (dir </> "dist-newstyle") `shouldReturn` True
            doesFileExist (dir </> "Main.hs") `shouldReturn` True
            doesDirectoryExist (dir </> "Cells") `shouldReturn` False
