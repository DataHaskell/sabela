{-# LANGUAGE OverloadedStrings #-}

{- | A hit that names a homepage raises "where do I read the source?" and, for
a package no index can describe, that is the only remaining answer. In the
2026-08-07 episode the model derived @jason-johnson/hodatime@ from the homepage
in its own head before it could call @list_files@; carrying the slug is the two
lines that spares it.
-}
module Test.DiscoverRepoSlugSpec (discoverRepoSlugSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.ModuleList (factKeys, repoSlugOf)

genSeg :: Gen Text
genSeg =
    T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_."))

discoverRepoSlugSpec :: Spec
discoverRepoSlugSpec = describe "the repo a homepage names (live 20260807)" $ do
    it "reads owner/name out of a GitHub homepage" $
        property $
            forAll ((,) <$> genSeg <*> genSeg) $ \(o, n) ->
                repoSlugOf ("https://github.com/" <> o <> "/" <> n)
                    `shouldBe` Just (o <> "/" <> n)

    it "reads the slug hodatime's own homepage states" $
        repoSlugOf "https://github.com/jason-johnson/hodatime"
            `shouldBe` Just "jason-johnson/hodatime"

    it "tolerates a trailing slash and a .git suffix" $ do
        repoSlugOf "https://github.com/a/b/" `shouldBe` Just "a/b"
        repoSlugOf "https://github.com/a/b.git" `shouldBe` Just "a/b"

    it "reads a slug from a scheme-less or www homepage" $ do
        repoSlugOf "github.com/a/b" `shouldBe` Just "a/b"
        repoSlugOf "http://www.github.com/a/b" `shouldBe` Just "a/b"

    {- list_files takes exactly "owner/name"; a deeper path is a page in the
    repo, not the repo, and guessing which prefix is the repo would be a
    guess the caller then has to check. -}
    it "declines a path deeper than the repository" $
        repoSlugOf "https://github.com/a/b/tree/main/src" `shouldBe` Nothing

    it "declines a host that is not GitHub" $
        property $
            forAll ((,) <$> genSeg <*> genSeg) $ \(o, n) ->
                repoSlugOf ("https://gitlab.com/" <> o <> "/" <> n)
                    `shouldBe` Nothing

    it "declines a homepage naming no repository at all" $ do
        repoSlugOf "https://github.com/a" `shouldBe` Nothing
        repoSlugOf "https://example.invalid/vapour" `shouldBe` Nothing
        repoSlugOf "" `shouldBe` Nothing

    {- The emitter and the schema live together in ModuleList precisely so a
    field cannot be emitted under a name the envelope will not declare. -}
    it "is a field the declared schema knows" $
        factKeys `shouldContain` ["repo"]
