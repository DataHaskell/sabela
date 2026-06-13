{-# LANGUAGE OverloadedStrings #-}

{- | Pins the session-staleness check: 'depsMatch' must notice changes to
local package dirs, git pins, and ghc-options — not just dep names and
extensions — so directive edits trigger a package-env rebuild.
-}
module Test.DepsMatchSpec (spec) where

import qualified Data.Set as S
import Sabela.Deps (ProjectSig (..), depsMatch, emptyProjectSig, projectSig)
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
    describe "depsMatch" $ do
        it "matches when needed deps are installed and sigs agree" $
            depsMatch
                emptyMeta{metaDeps = ["text"]}
                (S.fromList ["text"])
                S.empty
                S.empty
                emptyProjectSig
                emptyProjectSig
                `shouldBe` True

        it "keeps subset semantics: a dropped dep does not force a rebuild" $
            depsMatch
                emptyMeta{metaDeps = ["text"]}
                (S.fromList ["text", "containers"])
                S.empty
                S.empty
                emptyProjectSig
                emptyProjectSig
                `shouldBe` True

        it "rejects when a local package dir is added" $
            depsMatch
                emptyMeta
                S.empty
                S.empty
                S.empty
                (projectSig ["/code/dataframe"] emptyMeta)
                emptyProjectSig
                `shouldBe` False

        it "rejects when a local package dir is removed" $
            depsMatch
                emptyMeta
                S.empty
                S.empty
                S.empty
                emptyProjectSig
                (projectSig ["/code/dataframe"] emptyMeta)
                `shouldBe` False

        it "rejects when a git pin changes ref" $
            depsMatch
                emptyMeta
                S.empty
                S.empty
                S.empty
                (projectSig [] emptyMeta{metaSourceRepos = [pinAt]})
                (projectSig [] emptyMeta{metaSourceRepos = [pinAt{srpRef = "def456"}]})
                `shouldBe` False

        it "rejects when ghc-options change" $
            depsMatch
                emptyMeta
                S.empty
                S.empty
                S.empty
                (projectSig [] emptyMeta{metaGhcOptions = ["-fno-full-laziness"]})
                emptyProjectSig
                `shouldBe` False

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
