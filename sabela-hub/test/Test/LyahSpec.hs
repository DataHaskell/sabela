{-# LANGUAGE OverloadedStrings #-}

{- | The Learn You a Haskell converter: frontmatter handling, Pandoc-attribute
stripping (heading anchors, labelled spans, image attrs), image-path repointing,
and the GHCi-transcript splitter (runnable cells vs. static meta-command /
intentional-error blocks).
-}
module Test.LyahSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Hub.Gallery.Lyah (
    chapterFrontTitle,
    convertChapter,
    finalizeNotebook,
    neededDeps,
    stripPandocAttrs,
 )

-- | A small synthetic chapter exercising each conversion rule.
fixture :: T.Text
fixture =
    T.unlines
        [ "---"
        , "chapter: 1"
        , "title: \"Frontmatter Title\""
        , "---"
        , ""
        , "## Heading {#anchor}"
        , ""
        , "Some `code`{.label .function} prose."
        , ""
        , "![pic](assets/images/x/p.png){.right width=10}"
        , ""
        , "```{.haskell:hs}"
        , "f x = x + 1"
        , "```"
        , ""
        , "```{.haskell: .ghci}"
        , "ghci> 2 + 3"
        , "5"
        , "ghci> :t head"
        , "head :: [a] -> a"
        , "ghci> 5 + \"x\""
        , "<interactive>:1:1: error: bad"
        , "```"
        , ""
        , "```{.plain}"
        , "some shell output"
        , "```"
        ]

spec :: Spec
spec = do
    describe "chapterFrontTitle" $ do
        it "extracts the frontmatter title" $
            chapterFrontTitle fixture `shouldBe` Just "Frontmatter Title"
        it "returns Nothing without frontmatter" $
            chapterFrontTitle "## just a heading" `shouldBe` Nothing

    describe "stripPandocAttrs" $ do
        it "drops heading anchors" $
            stripPandocAttrs "## So What {#so-what}" `shouldBe` "## So What"
        it "drops labelled spans, keeping the code" $
            stripPandocAttrs "use `head`{.label .function} here"
                `shouldBe` "use `head` here"
        it "leaves ordinary braces alone" $
            stripPandocAttrs "Foo {bar = 1}" `shouldBe` "Foo {bar = 1}"

    describe "convertChapter" $ do
        let out = convertChapter "Chapter One" fixture

        it "prepends the title and CC BY-NC-SA attribution" $ do
            ("# Chapter One" `T.isInfixOf` out) `shouldBe` True
            ("CC BY-NC-SA 3.0" `T.isInfixOf` out) `shouldBe` True

        it "drops the YAML frontmatter" $
            ("chapter: 1" `T.isInfixOf` out) `shouldBe` False

        it "strips heading anchors and labelled spans in prose" $ do
            ("## Heading\n" `T.isInfixOf` out) `shouldBe` True
            ("{#anchor}" `T.isInfixOf` out) `shouldBe` False
            ("Some `code` prose." `T.isInfixOf` out) `shouldBe` True

        it "repoints image paths at the public asset host" $ do
            ("](https://raw.githubusercontent.com/learnyouahaskell" `T.isInfixOf` out)
                `shouldBe` True
            ("](assets/" `T.isInfixOf` out) `shouldBe` False

        it "turns a .haskell:hs block into a runnable haskell cell" $
            ("```haskell\nf x = x + 1\n```" `T.isInfixOf` out) `shouldBe` True

        it "makes a ghci expression a runnable cell (prompt stripped)" $
            ("```haskell\n2 + 3\n```" `T.isInfixOf` out) `shouldBe` True

        it "keeps a ghci meta-command as a static text block" $
            ("```text\nghci> :t head\nhead :: [a] -> a\n```" `T.isInfixOf` out)
                `shouldBe` True

        it "keeps an intentional-error example as a static text block" $ do
            ("```text\nghci> 5 + \"x\"\n<interactive>:1:1: error: bad\n```" `T.isInfixOf` out)
                `shouldBe` True
            -- the erroring input is NOT emitted as a runnable cell
            ("```haskell\n5 + \"x\"\n```" `T.isInfixOf` out) `shouldBe` False

        it "renders a .plain block as static text" $
            ("```text\nsome shell output\n```" `T.isInfixOf` out) `shouldBe` True

    describe "neededDeps" $ do
        it "maps Data.Map to containers" $
            neededDeps "import qualified Data.Map as M" `shouldBe` ["containers"]
        it "dedupes and covers random/mtl/directory" $
            neededDeps "import System.Random\nimport Control.Monad.State\nData.Set"
                `shouldBe` ["containers", "random", "mtl"]
        it "is empty for a base-only chapter" $
            neededDeps "import Data.List\nimport Data.Char" `shouldBe` []

    describe "convertChapter cabal setup cell" $
        it "prepends a build-depends cell when extra packages are needed" $ do
            let md =
                    "---\ntitle: \"X\"\n---\n\n```{.haskell:hs}\nimport qualified Data.Map as M\n```"
            ("-- cabal: build-depends: base, containers" `T.isInfixOf` convertChapter "X" md)
                `shouldBe` True

    describe "finalizeNotebook" $ do
        it "downgrades an errored cell to static text and drops the error output" $ do
            let nb =
                    T.intercalate
                        "\n"
                        [ "```haskell"
                        , "Map.fromList [(1,2)]"
                        , "```"
                        , ""
                        , "> <!-- scripths:mime text/plain -->"
                        , "> <interactive>:1:1: error: Not in scope"
                        ]
                (out, n) = finalizeNotebook nb
            n `shouldBe` 1
            ("```text\nMap.fromList [(1,2)]\n```" `T.isInfixOf` out) `shouldBe` True
            ("error:" `T.isInfixOf` out) `shouldBe` False

        it "keeps a cell whose output is clean" $ do
            let nb = "```haskell\n2 + 2\n```\n\n> <!-- scripths:mime text/plain -->\n> 4"
                (out, n) = finalizeNotebook nb
            n `shouldBe` 0
            ("```haskell\n2 + 2\n```" `T.isInfixOf` out) `shouldBe` True
            ("> 4" `T.isInfixOf` out) `shouldBe` True
