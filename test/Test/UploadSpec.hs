{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the upload filename guard. The handler also canonicalizes and
confines the target directory; here we pin the pure name-sanitization that
stops a crafted file name from escaping its directory.
-}
module Test.UploadSpec (spec) where

import Sabela.Server (safeUploadName)
import Test.Hspec

spec :: Spec
spec = describe "safeUploadName" $ do
    it "keeps a plain file name" $
        safeUploadName "data.csv" `shouldBe` Just "data.csv"
    it "reduces a path to its basename" $
        safeUploadName "a/b/c.csv" `shouldBe` Just "c.csv"
    it "defeats parent-dir traversal in the name" $
        safeUploadName "../../etc/passwd" `shouldBe` Just "passwd"
    it "trims surrounding whitespace" $
        safeUploadName "  notes.txt  " `shouldBe` Just "notes.txt"
    it "rejects empty, dot, dotdot, and path-only names" $ do
        safeUploadName "" `shouldBe` Nothing
        safeUploadName "." `shouldBe` Nothing
        safeUploadName ".." `shouldBe` Nothing
        safeUploadName "a/b/" `shouldBe` Nothing
