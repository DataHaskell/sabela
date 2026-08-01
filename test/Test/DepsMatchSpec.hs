{-# LANGUAGE OverloadedStrings #-}

module Test.DepsMatchSpec (spec) where

import qualified Data.Set as S
import Sabela.Deps (
    ProjectSig (..),
    emptyProjectSig,
    envSig,
    projectSig,
 )
import ScriptHs.Parser (CabalMeta (..), SourceRepoPin (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

emptyMeta :: CabalMeta
emptyMeta =
    CabalMeta
        { metaDeps = []
        , metaExts = []
        , metaGhcOptions = []
        , metaExtraLibDirs = []
        , metaExtraIncludeDirs = []
        , metaPackages = []
        , metaSourceRepos = []
        , metaUnknownKeys = []
        }

pinAt :: SourceRepoPin
pinAt =
    SourceRepoPin
        { srpLocation = "https://github.com/example/repo"
        , srpRef = "abc123"
        , srpSubdir = Nothing
        }

spec :: Spec
spec = do
    describe "envSig (what a kernel was built from)" $ do
        let sigOf globals = envSig (S.fromList globals)
            withDeps ds = emptyMeta{metaDeps = ds}
        it "is equal for two notebooks needing the same environment" $
            sigOf [] [] (withDeps ["text"]) `shouldBe` sigOf [] [] (withDeps ["text"])
        it
            "changes when a dependency is REMOVED, which a subset test cannot see"
            $ sigOf [] [] (withDeps ["text"])
                `shouldNotBe` sigOf [] [] (withDeps ["text", "containers"])
        it "changes when a dependency is added" $
            sigOf [] [] (withDeps ["text"])
                `shouldNotBe` sigOf [] [] (withDeps ["text", "aeson"])
        it "excludes globally-provided deps, which no kernel installs" $
            sigOf ["text"] [] (withDeps ["text"]) `shouldBe` sigOf ["text"] [] emptyMeta
        it "distinguishes extensions" $
            sigOf [] [] emptyMeta{metaExts = ["GADTs"]}
                `shouldNotBe` sigOf [] [] emptyMeta
        it "distinguishes the local package overlay" $
            sigOf [] ["../sabela-notebook"] emptyMeta
                `shouldNotBe` sigOf [] [] emptyMeta
        it "distinguishes a source-repository pin" $
            sigOf [] [] emptyMeta{metaSourceRepos = [pinAt]}
                `shouldNotBe` sigOf [] [] emptyMeta

    describe "projectSig" $ do
        it "is insensitive to local package dir order" $
            projectSig ["/b", "/a"] emptyMeta
                `shouldBe` projectSig ["/a", "/b"] emptyMeta

        it "is insensitive to git pin order" $ do
            let other = pinAt{srpLocation = "https://github.com/example/two"}
            projectSig [] emptyMeta{metaSourceRepos = [pinAt, other]}
                `shouldBe` projectSig [] emptyMeta{metaSourceRepos = [other, pinAt]}

        it "keeps ghc-options order significant" $
            projectSig [] emptyMeta{metaGhcOptions = ["-a", "-b"]}
                `shouldNotBe` projectSig [] emptyMeta{metaGhcOptions = ["-b", "-a"]}
