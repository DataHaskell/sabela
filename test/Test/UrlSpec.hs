{-# LANGUAGE OverloadedStrings #-}

module Test.UrlSpec (spec) where

import Sabela.Url (rewriteGitHubUrl)
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
