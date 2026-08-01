{-# LANGUAGE OverloadedStrings #-}

module Test.WorkspaceRecoverySpec (spec) where

import Sabela.Session.Workspace (
    buildIsDirty,
    clearBuildDirty,
    markBuildDirty,
    wipeBuildArtifacts,
 )
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

-- | A project dir shaped like one a build leaves behind.
withProject :: (FilePath -> IO a) -> IO a
withProject act = withSystemTempDirectory "sabela-workspace" $ \dir -> do
    createDirectoryIfMissing True (dir </> "dist-newstyle" </> "build")
    createDirectoryIfMissing True (dir </> "ghci-objs")
    writeFile (dir </> "dist-newstyle" </> "build" </> "Main.hi") "truncated"
    writeFile (dir </> "ghci-objs" </> "Main.o") "truncated"
    writeFile (dir </> "cabal.project") "packages: ."
    writeFile (dir </> "Main.hs") "main = pure ()"
    act dir

spec :: Spec
spec = do
    describe "the non-graceful-death marker" $ do
        it "a fresh project is not dirty" $
            withProject $ \dir -> buildIsDirty dir `shouldReturn` False
        it "spawning marks it, so a kill leaves evidence behind" $
            withProject $ \dir -> do
                markBuildDirty dir
                buildIsDirty dir `shouldReturn` True
        it "a graceful close clears it" $
            withProject $ \dir -> do
                markBuildDirty dir
                clearBuildDirty dir
                buildIsDirty dir `shouldReturn` False
        it "clearing an unmarked project is not an error" $
            withProject $ \dir -> do
                clearBuildDirty dir
                buildIsDirty dir `shouldReturn` False

    describe "wipeBuildArtifacts" $ do
        it "removes the trees a killed build can leave inconsistent" $
            withProject $ \dir -> do
                wipeBuildArtifacts dir
                doesDirectoryExist (dir </> "dist-newstyle") `shouldReturn` False
                doesDirectoryExist (dir </> "ghci-objs") `shouldReturn` False
        it "keeps the project itself, which is cheap to reuse and safe" $
            withProject $ \dir -> do
                wipeBuildArtifacts dir
                doesFileExist (dir </> "cabal.project") `shouldReturn` True
                doesFileExist (dir </> "Main.hs") `shouldReturn` True
        it "is a no-op on a project that has never been built" $
            withSystemTempDirectory "sabela-empty" $ \dir -> do
                wipeBuildArtifacts dir
                doesDirectoryExist dir `shouldReturn` True

    describe "recovery decision" $
        it
            "a project left dirty by a killed build wipes; a cleanly closed one\
            \ keeps its artefacts, so the common case stays fast"
            $ withProject
            $ \dir -> do
                markBuildDirty dir
                dirty <- buildIsDirty dir
                dirty `shouldBe` True
                wipeBuildArtifacts dir
                clearBuildDirty dir
                buildIsDirty dir `shouldReturn` False
