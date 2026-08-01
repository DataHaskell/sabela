{-# LANGUAGE OverloadedStrings #-}

module Test.GitHubSpec (spec) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import Test.Hspec

import Sabela.AI.GitHub (
    GhEntry (..),
    parseTree,
    rawUrl,
    repoSlug,
    treeUrl,
 )

treeJson :: Value
treeJson =
    fromMaybe (error "bad fixture") . decode . LBS.pack $
        "{\"tree\":[\
        \{\"path\":\"Data\",\"type\":\"tree\"},\
        \{\"path\":\"Data/Map.hs\",\"type\":\"blob\",\"size\":1200},\
        \{\"path\":\"README.md\",\"type\":\"blob\",\"size\":40}\
        \],\"truncated\":false}"

spec :: Spec
spec = describe "Sabela.AI.GitHub" $ do
    describe "repoSlug" $ do
        it "accepts owner/name" $
            repoSlug "haskell/containers" `shouldBe` Right "haskell/containers"

        it "rejects a slug with no owner" $
            repoSlug "containers" `shouldSatisfy` isLeft

        it "rejects a slug with extra segments" $
            repoSlug "a/b/c" `shouldSatisfy` isLeft

        it "rejects path traversal in the slug" $
            repoSlug "../etc" `shouldSatisfy` isLeft

    describe "treeUrl" $ do
        it "defaults the ref to HEAD and asks for the whole tree" $
            treeUrl "haskell/containers" Nothing
                `shouldBe` "https://api.github.com/repos/haskell/containers/git/trees/HEAD?recursive=1"

        it "uses an explicit ref" $
            treeUrl "haskell/containers" (Just "0.6.7")
                `shouldBe` "https://api.github.com/repos/haskell/containers/git/trees/0.6.7?recursive=1"

    describe "rawUrl" $ do
        it "defaults the ref to HEAD" $
            rawUrl "haskell/containers" Nothing "Data/Map.hs"
                `shouldBe` "https://raw.githubusercontent.com/haskell/containers/HEAD/Data/Map.hs"

        it "uses an explicit ref" $
            rawUrl "haskell/containers" (Just "0.6.7") "Data/Map.hs"
                `shouldBe` "https://raw.githubusercontent.com/haskell/containers/0.6.7/Data/Map.hs"

    describe "parseTree" $ do
        it "keeps blobs and drops trees" $
            fmap (map ghPath . fst) (parseTree treeJson)
                `shouldBe` Right ["Data/Map.hs", "README.md"]

        it "carries the blob size" $
            fmap (map ghSize . fst) (parseTree treeJson)
                `shouldBe` Right [Just 1200, Just 40]

        it "reports whether GitHub truncated the tree" $
            fmap snd (parseTree treeJson) `shouldBe` Right False

        it "fails on a response that is not a tree" $
            parseTree "not a tree" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
