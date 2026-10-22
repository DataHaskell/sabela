{-# LANGUAGE OverloadedStrings #-}

module Test.UrlSpec (spec) where

import Sabela.Url (rewriteDataUrl, rewriteGitHubUrl)
import Test.Hspec

spec :: Spec
spec = describe "rewriteGitHubUrl" $ do
    it "rewrites a github blob URL to raw.githubusercontent.com" $
        rewriteGitHubUrl
            "https://github.com/o/r/blob/main/notebook.md"
            `shouldBe` "https://raw.githubusercontent.com/o/r/main/notebook.md"
    it "keeps nested paths under the ref" $
        rewriteGitHubUrl
            "https://github.com/o/r/blob/main/dir/sub/n.md"
            `shouldBe` "https://raw.githubusercontent.com/o/r/main/dir/sub/n.md"
    it "drops a #Lxx fragment from a blob URL" $
        rewriteGitHubUrl
            "https://github.com/o/r/blob/main/n.md#L42"
            `shouldBe` "https://raw.githubusercontent.com/o/r/main/n.md"
    it "drops a ?raw=true query from a blob URL" $
        rewriteGitHubUrl
            "https://github.com/o/r/blob/main/n.md?raw=true"
            `shouldBe` "https://raw.githubusercontent.com/o/r/main/n.md"
    it "appends /raw to a bare gist URL" $
        rewriteGitHubUrl
            "https://gist.github.com/user/abc123"
            `shouldBe` "https://gist.github.com/user/abc123/raw"
    it "leaves an already-raw gist URL untouched" $
        rewriteGitHubUrl
            "https://gist.github.com/user/abc123/raw"
            `shouldBe` "https://gist.github.com/user/abc123/raw"
    it "leaves a raw.githubusercontent.com URL untouched" $
        rewriteGitHubUrl
            "https://raw.githubusercontent.com/o/r/main/n.md"
            `shouldBe` "https://raw.githubusercontent.com/o/r/main/n.md"
    it "passes a non-GitHub URL through unchanged (keeps its query)" $
        rewriteGitHubUrl
            "https://example.com/data.csv?token=xyz"
            `shouldBe` "https://example.com/data.csv?token=xyz"
    it "trims surrounding whitespace" $
        rewriteGitHubUrl
            "  https://example.com/n.md  "
            `shouldBe` "https://example.com/n.md"
    it "does not rewrite a github tree (directory) URL" $
        rewriteGitHubUrl
            "https://github.com/o/r/tree/main/dir"
            `shouldBe` "https://github.com/o/r/tree/main/dir"

    describe "rewriteDataUrl" $ do
        it "maps s3:// to the bucket's public HTTPS endpoint" $
            rewriteDataUrl "s3://my-bucket/dir/sales.csv"
                `shouldBe` "https://my-bucket.s3.amazonaws.com/dir/sales.csv"
        it "turns a Drive share link into a download link" $
            rewriteDataUrl "https://drive.google.com/file/d/ABC123/view?usp=sharing"
                `shouldBe` "https://drive.google.com/uc?export=download&id=ABC123"
        it "turns a Drive open?id= link into a download link" $
            rewriteDataUrl "https://drive.google.com/open?id=XYZ"
                `shouldBe` "https://drive.google.com/uc?export=download&id=XYZ"
        it "exports a Google Sheet as CSV, keeping its tab" $
            rewriteDataUrl "https://docs.google.com/spreadsheets/d/SHEET/edit#gid=42"
                `shouldBe` "https://docs.google.com/spreadsheets/d/SHEET/export?format=csv&gid=42"
        it "still rewrites GitHub blob links" $
            rewriteDataUrl "https://github.com/o/r/blob/main/d.csv"
                `shouldBe` "https://raw.githubusercontent.com/o/r/main/d.csv"
